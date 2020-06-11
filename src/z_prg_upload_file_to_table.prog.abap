*&---------------------------------------------------------------------*
*& Report Z_PRG_UPLOAD_FILE_TO_TABLE
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
report z_prg_upload_file_to_table.

*--------------------------------------------------------------------*
* Global data
*--------------------------------------------------------------------*
tables: sscrfields.

data: gv_table type rsrd1-tbma_val.
field-symbols: <gt_excel> type standard table,
               <gt>       type standard table.
*--------------------------------------------------------------------*
* Selection screen
*--------------------------------------------------------------------*
selection-screen begin of block sel with frame title text-sel.
parameters: p_table like gv_table obligatory.
selection-screen: begin of line,
                  comment 1(70) text-cmt modif id cmt,
                  end of line.
selection-screen end of block sel.

selection-screen begin of block fil with frame title text-fil.
parameters: p_file type rlgrap-filename.
selection-screen end of block fil.

selection-screen begin of block dwn with frame title text-dwn.
selection-screen:
  begin of line,
    pushbutton 2(25) text-dwn user-command dwn modif id dwn,
    position 32.
parameters: c_wdata as checkbox modif id dwn.
selection-screen:
    comment 35(40) text-dat modif id dwn,
  end of line.
selection-screen end of block dwn.

selection-screen function key: 1, 2.
*--------------------------------------------------------------------*
* local class definitions
*--------------------------------------------------------------------*
class lcl_app definition.
  public section.
    class-methods:
      set_sel_screen_functions,
      f4_file,
      screen_modif,
      input_fld_check,
      process_ucomm,
      create_table,
      build_excel_tab.

    methods: process.

  protected section.
    " placeholder

  private section.
    class-data: mv_table like p_table.
    methods: read_file_data.
endclass.

class lcl_main definition.
  public section.
    class-methods: start.

  protected section.
    " placeholder

  private section.
    " placeholder
endclass.
*--------------------------------------------------------------------*
* local class implementation
*--------------------------------------------------------------------*
class lcl_app implementation.
  method set_sel_screen_functions.
*    if <gt> is assigned.
    data(ls_functxt) = value smp_dyntxt( icon_id   = icon_list
                                         quickinfo = 'Display Table Data'
                                         icon_text = '' ).

    sscrfields-functxt_01 = ls_functxt.  " 01, 02, 03, 04, 05

    clear ls_functxt.
    ls_functxt = value smp_dyntxt( icon_id   = icon_delete
                                   quickinfo = 'Delete Table Data'
                                   icon_text = '' ).

    sscrfields-functxt_02 = ls_functxt.  " 01, 02, 03, 04, 05
*    endif.
  endmethod.

  method f4_file.
    data: lv_file_filter type string,
          lt_filein      type filetable,
          lv_rc          type i,
          lv_user_action type i.

    clear: lv_file_filter, lv_rc, lv_user_action.
    refresh: lt_filein.
    move 'Excel files(*.xls,*.xlsx)|*.xls*' to lv_file_filter. " description|*.extension

    cl_gui_frontend_services=>file_open_dialog(
      exporting
        file_filter             = lv_file_filter
        multiselection          = space
      changing
        file_table              = lt_filein
        rc                      = lv_rc         " no of files selected
        user_action             = lv_user_action
      exceptions
        file_open_dialog_failed = 1
        cntl_error              = 2
        error_no_gui            = 3
        not_supported_by_gui    = 4
        others                  = 5 ).

    if sy-subrc <> 0.
      message id sy-msgid type 'S' number sy-msgno with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 display like 'E'.
      return.
    endif.

    if lv_user_action eq 9.
      " action cancelled
      message 'Action cancelled by user' type 'S' display like 'E'.
      return.
    endif.

    if lt_filein is not initial and lv_rc gt 0 and lv_user_action eq 0.
      try.
          p_file = conv #( lt_filein[ 1 ]-filename ).
        catch cx_sy_itab_line_not_found.
      endtry.
    else.
      message 'Input file not selected' type 'I' display like 'E'.
      clear p_file.
      return.
    endif.
  endmethod.

  method screen_modif.
    loop at screen.
      if screen-group1 eq 'CMT'.
        screen-intensified = '1'.
      endif.
      if screen-group1 eq 'DWN'.
        if p_table is not initial.
          screen-active = '1'.
        else.
          screen-active = '0'.
        endif.
      endif.
      modify screen.
    endloop.
  endmethod.

  method input_fld_check.
    " check if table exists
    if not cl_rebf_ddic_tabl=>exists(
        exporting
          id_name = conv #( to_upper( p_table ) )
          id_tabclass = conv #( 'TRANSP' )
          if_noview = abap_true ).
      message 'Table does not exist or is not transparent' type 'E'.
    endif.
    " check customer namespace
    if p_table is not initial.
      if p_table np 'Y*' and p_table np 'Z*' and p_table ns 'WITS/'. " Allow exalca objects
        message 'Table does not lie in customer namespace' type 'E'.
      endif.
    endif.

    if mv_table <> p_table.
      mv_table = p_table.
      clear p_file.
    endif.

    create_table( ).
  endmethod.

  method process_ucomm.
    if sy-ucomm eq 'ONLI'.
      if p_file is initial.
        set cursor field 'P_FILE'.
        message e055(00).
      endif.
    endif.
    if sy-ucomm eq 'DWN'.
      " build and download file format
      create_table( ).
      if <gt> is assigned.
        build_excel_tab( ).

        if <gt_excel> is not initial.
          data: lv_file_path type string,
                lv_filename  type string,
                lv_path      type string.

          clear: lv_path, lv_filename, lv_file_path.
          cl_gui_frontend_services=>file_save_dialog(
            exporting
              window_title      = conv #( |Download file format| )
              "<<< IHDK904352: XX: S_K: PRG: Upl file 2 tab: Fix invalid filename: 23.12.19 >>>
              " replace invalid characters in filename with '-' : Eg. /WITS/Table ==> -WITS-Table
              default_file_name = conv #( |File_for_{ translate( val = p_table from = '<>:"/\|?*' to = '---------' ) }| )
              default_extension = conv #( |xls| )
            changing
              filename          = lv_filename
              path              = lv_path
              fullpath          = lv_file_path
            exceptions
              cntl_error                = 1
              error_no_gui              = 2
              not_supported_by_gui      = 3
              invalid_default_file_name = 4
              others = 5 ).

          if sy-subrc <> 0.
            message id sy-msgid type 'S' number sy-msgno with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 display like 'E'.
          endif.

          check lv_file_path is not initial and sy-subrc = 0.

          if c_wdata eq abap_true.
            data: lt type ref to data.
            field-symbols: <lt> type standard table.

            create data lt type table of (p_table).
            if lt is bound.
              assign lt->* to <lt>.
              if <lt> is assigned.
                try.
                    select * from (p_table) into corresponding fields of table <lt>.

                    try.
                        cl_salv_table=>factory(
                          importing
                            r_salv_table   = data(lo_alv)              " Basis Class Simple ALV Tables
                          changing
                            t_table        = <lt> ).

                        data(lo_columns) = lo_alv->get_columns( ).
                        if lo_columns is bound.
                          data(lt_col) = lo_columns->get( ).

                          if lt_col is not initial.
                            loop at lt_col into data(ls_col).
                              ls_col-r_column->set_long_text( exporting value = conv #( ls_col-columnname ) ).
                              ls_col-r_column->set_medium_text( exporting value = conv #( ls_col-columnname ) ).
                              ls_col-r_column->set_short_text( exporting value = conv #( ls_col-columnname ) ).
                              clear ls_col.
                            endloop.
                          endif.

                          lo_columns->set_optimize( exporting value = if_salv_c_bool_sap=>true ).
                        endif.

                        data(lv_xml) = lo_alv->to_xml(
                             exporting
                               xml_type    = if_salv_bs_xml=>c_type_xlsx
                               xml_flavour = if_salv_bs_c_tt=>c_tt_xml_flavour_export ).

                        if lv_xml is not initial.

                          try.
                              zcl_helper=>write_file_to_path(
                                exporting
                                  iv_filepath    = lv_file_path    " Path of file to write to on frontend or app server
                                  iv_file_length = xstrlen( lv_xml ) " Size of binary data
                                  it_data        = cl_bcs_convert=>xstring_to_solix( exporting iv_xstring = lv_xml ) ).       " Binary Data
                            catch zcx_generic ##no_handler. " Generic Exception Class
                          endtry.
                        endif.
                      catch cx_salv_msg. " ALV: General Error Class with Message
                    endtry.
*--------------------------------------------------------------------*
*                    <gt_excel> = corresponding #( base ( <gt_excel> ) <lt> ).
*
*                    data(lt_component_db) = cast cl_abap_structdescr(
*                                              cast cl_abap_tabledescr(
*                                                cl_abap_typedescr=>describe_by_data(
*                                                  exporting
*                                                    p_data = <lt> ) )->get_table_line_type( ) )->components.
*
*                    data(lt_component_ex) = cast cl_abap_structdescr(
*                                              cast cl_abap_tabledescr(
*                                                cl_abap_typedescr=>describe_by_data(
*                                                  exporting
*                                                    p_data = <lt> ) )->get_table_line_type( ) )->components.
*
*                    loop at <gt_excel> assigning field-symbol(<ls_excel>) from 2.
*                      loop at lt_component_ex into data(ls_component_ex).
*                        assign component ls_component_ex-name of structure <ls_excel> to field-symbol(<lv_ex>).
*                        try.
*                            data(ls_component_db) = lt_component_db[ name = ls_component_ex-name ].
*                            if ls_component_db-type_kind = cl_abap_typedescr=>typekind_date.
*                              zcl_helper=>conv_exit(
*                                exporting
*                                  iv      = conv sy-datum( <lv_ex> )  " Unconverted Input
*                                  iv_mode = 'O' " Mode: I = Input, O = Output
*                                importing
*                                  ev      = <lv_ex> ). " Converted Output
*                            endif.
*                            if ls_component_db-type_kind = cl_abap_typedescr=>typekind_time.
*                              zcl_helper=>conv_exit(
*                                exporting
*                                  iv      = conv sy-uzeit( <lv_ex> )  " Unconverted Input
*                                  iv_mode = 'O' " Mode: I = Input, O = Output
*                                importing
*                                  ev      = <lv_ex> ). " Converted Output
*                            endif.
*                          catch cx_sy_itab_line_not_found ##no_handler.
*                        endtry.
*
*                        clear:
*                          ls_component_ex,
*                          ls_component_db.
*
*                        unassign <lv_ex>.
*                      endloop.
*                    endloop.
*
*                    zcl_helper=>itab_to_excel(
*                      exporting
*                        it_itab             = <gt_excel>            " Single internal table to be converted
*                        iv_insert_header    = abap_false            " Add header line
*                        iv_force_string     = abap_true             " Convert all values to string
*                        iv_file_path        = lv_file_path ).       " Filepath on frontend or app server to download to...
*--------------------------------------------------------------------*
                  catch cx_sy_dynamic_osql_error into data(lox_dyn_sql).
                endtry.
              endif.
            endif.
          endif.

          if zcl_helper=>check_file_exists(
               exporting
                 iv_filepath = conv #( lv_file_path ) ). " File path to check

            p_file = lv_file_path.

            cl_gui_frontend_services=>execute(
              exporting
                document               = conv #( lv_file_path )       " Path+Name to Document
                maximized              = conv #( abap_true )          " Show Window Maximized
              exceptions
                cntl_error             = 1                 " Control error
                error_no_gui           = 2                 " No GUI available
                bad_parameter          = 3                 " Incorrect parameter combination
                file_not_found         = 4                 " File not found
                path_not_found         = 5                 " Path not found
                file_extension_unknown = 6                 " Could not find application for specified extension
                error_execute_failed   = 7                 " Could not execute application or document
                synchronous_failed     = 8                 " Cannot Call Application Synchronously
                not_supported_by_gui   = 9                 " GUI does not support this
                others                 = 10 ).

            if sy-subrc <> 0.
              message id sy-msgid type sy-msgty number sy-msgno
                with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
            endif.
          endif.

          refresh <gt_excel>.
          if sy-subrc <> 0.
            message id sy-msgid type 'S' number sy-msgno with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 display like 'E'.
          else.
            message 'File format downloaded successfully at' && ` ` && lv_file_path type 'S'.
          endif.
        else.
          message 'Error generating excel file format' type 'S' display like 'E'.
        endif.
      endif.
    endif.

    if sy-ucomm = 'FC01'.
      if <gt> is assigned.
        data(lv_subrc) = value sy-subrc( ).
        perform xx_check_tcode in program saplsd41 using 'SE16' changing lv_subrc.
        if lv_subrc = 0.
          call function 'RS_TOOL_ACCESS'
            exporting
              operation   = 'TAB_CONT'
              object_name = conv rsdxx-objname( p_table )
              object_type = conv rsdxx-trobjtype( 'TABL' ).
        endif.
      endif.
    endif.

    if sy-ucomm = 'FC02'.
      if <gt> is assigned.

        data lv_data_exists type abap_bool.
        clear lv_data_exists.
        select single @abap_true
          from (p_table)
          into @lv_data_exists.

        if lv_data_exists = abap_true.
          data lv_answer type c length 1.
          clear lv_answer.

          call function 'POPUP_TO_CONFIRM'
            exporting
              titlebar       = 'Content Deletion Warning!'
              text_question  = |All the data in the table { p_table } will be deleted. Do you wish to proceed?|
            importing
              answer         = lv_answer
            exceptions
              text_not_found = 1
              others         = 2.
          if sy-subrc <> 0.
          endif.

          if lv_answer = '1'.
            delete from (p_table).

            if sy-subrc = 0 or sy-dbcnt > 0.
              commit work.

              message 'Data deleted successfully.' type 'S'.
            endif.
          endif.
        else.
          message i718(ad) with p_table.
        endif.
      endif.
    endif.
  endmethod.

  method build_excel_tab.
    if <gt> is assigned.
      data(lo_table_descr) = cast cl_abap_tabledescr( cl_abap_typedescr=>describe_by_data( <gt> ) ).
      if lo_table_descr is bound.
        data(lo_struct_descr) = cast cl_abap_structdescr( lo_table_descr->get_table_line_type( ) ).
      endif.
      if lo_struct_descr is bound.
        data(lt_component) = lo_struct_descr->components.
*        loop at lt_component into data(ls_component) where as_include = abap_true.
*          try.
*              lo_struct_descr ?= ls_component-type.
*              append lines of lo_struct_descr->get_components( ) to lt_component.
*            catch cx_sy_move_cast_error ##no_handler.
*          endtry.
*          clear ls_component.
*        endloop.
*
*        delete lt_component where as_include = abap_true.
      endif.

      data: begin of ls_flds,
              fname   type abap_componentdescr-name,
              flength type i,
            end of ls_flds,
            lt_flds like standard table of ls_flds.

      refresh lt_flds.
      if lt_component is not initial.
        loop at lt_component into data(ls_component).
          clear ls_flds.

          ls_flds-fname = ls_component-name.
          ls_flds-flength = ls_component-length.
          append ls_flds to lt_flds.

          clear ls_component.
        endloop.
      endif.

      if lt_flds is not initial.
        sort lt_flds descending by flength.
        try.
            try.
                if line_exists( lt_component[ type_kind = cl_abap_typedescr=>typekind_string ] ).
                  data(lo_max_elem) = cl_abap_elemdescr=>get_string( ).
                else.
                  lo_max_elem = cl_abap_elemdescr=>get_c( exporting p_length = cond #( when lt_flds[ 1 ]-flength >= 50
                                                                                       then lt_flds[ 1 ]-flength
                                                                                       else 50 ) ). " field with max length
                endif.
              catch cx_parameter_invalid_range.
            endtry.

            if lo_max_elem is bound.
              data: lo_max type ref to data.
              field-symbols: <lv_max> type any.

              free: lo_max.
              unassign <lv_max>.
              create data lo_max type handle lo_max_elem.
              assign lo_max->* to <lv_max>.

              if <lv_max> is assigned.
                data: lt_comp_create type cl_abap_structdescr=>component_table.
                refresh lt_comp_create.
                clear ls_component.
                loop at lt_component into ls_component.
                  append value #( name = ls_component-name
                                  type = cast cl_abap_datadescr( cl_abap_typedescr=>describe_by_data( <lv_max> ) ) ) to lt_comp_create.
                  clear ls_component.
                endloop.
              endif.
            endif.
          catch cx_sy_itab_line_not_found.
        endtry.
      endif.

      if lt_comp_create is not initial.
        try.
            data(lo_struct) = cl_abap_structdescr=>create(
                               exporting
                                 p_components = lt_comp_create ).
          catch cx_sy_struct_creation.
        endtry.

        data: ls_table type ref to data.
        create data ls_table type handle lo_struct.

        try.
            data(lo_tabletype) = cl_abap_tabledescr=>create(
                                  exporting
                                    p_line_type = cast cl_abap_structdescr( cl_abap_typedescr=>describe_by_data_ref( ls_table ) )
                                    p_table_kind = cl_abap_tabledescr=>tablekind_std ).
          catch cx_sy_table_creation.
        endtry.

        if lo_tabletype is bound.
          data: lt_table type ref to data.
          create data lt_table type handle lo_tabletype.

          unassign <gt_excel>.
          if lt_table is bound.
            assign lt_table->* to <gt_excel>.
          endif.
          if <gt_excel> is assigned.
            append initial line to <gt_excel> assigning field-symbol(<ls_excel>).
            do.
              assign component sy-index of structure <ls_excel> to field-symbol(<lv>).
              if sy-subrc <> 0.
                exit.
              endif.
              if <lv> is assigned.
                try.
                    <lv> = lt_component[ sy-index ]-name.
                    unassign <lv>.
                  catch cx_sy_itab_line_not_found.
                endtry.
              endif.
            enddo.
          else.
            message 'Error generating excel file format' type 'S' display like 'E'.
          endif.
        endif.
      endif.
    endif.
  endmethod.

  method process.
    if p_file is not initial.
      if p_table is not initial.
        create_table( ).
        if <gt> is assigned.
          read_file_data( ).

          if <gt> is not initial.
            modify (p_table) from table <gt>.
            if sy-dbcnt ge 1.
              commit work.
              message 'Data uploaded successfully' type 'S'.
            else.
              rollback work.
              message 'Error uploading data' type 'S' display like 'E'.
            endif.
          else.
            message 'Error reading data from file' type 'S' display like 'E'.
          endif.
        endif.
      endif.
    endif.
  endmethod.

  method create_table.
    data: lo type ref to data.

    free: lo.
    unassign <gt>.
    create data lo type table of (p_table).
    assign lo->* to <gt>.

    if <gt> is assigned.
      refresh <gt>.
    else.
      message 'Table could not be created' type 'S' display like 'E'.
    endif.
  endmethod.

  method read_file_data.
    if <gt_excel> is not assigned.
      build_excel_tab( ).
    endif.
    check <gt_excel> is assigned and p_file is not initial and <gt> is assigned.
    refresh: <gt_excel>, <gt>.

    zcl_helper=>excel_to_itab(
      exporting
        iv_file               = conv #( p_file )         " Local file for upload/download
        iv_check_file_format  = abap_true                " Check file field sequence
      changing
        ct_itab               = <gt_excel> ).            " Internal table

    check <gt_excel> is not initial.
    if p_table is not initial.
      if <gt> is assigned.
        loop at <gt_excel> assigning field-symbol(<ls_excel>).
          append initial line to <gt> assigning field-symbol(<ls>).
          if <ls> is assigned.
            zcl_helper=>format_excel_to_bapi( changing is_excel = <ls_excel> is_data = <ls> ).
            assign component 'MANDT' of structure  <ls> to field-symbol(<lv>).
            if <lv> is assigned and <lv> is initial.
              <lv> = sy-mandt.
            endif.
            unassign <lv>.
          endif.
          unassign <ls>.
        endloop.
      endif.
    endif.
  endmethod.
endclass.

class lcl_main implementation.
  method start.
    try.
        new lcl_app( )->process( ).
      catch cx_root into data(lox_root).
        message lox_root->get_text( ) type 'S' display like 'E'.
        return.
    endtry.
  endmethod.
endclass.
*--------------------------------------------------------------------*
* pre-selection screen events
*--------------------------------------------------------------------*
load-of-program.
  " placeholder

initialization.
  lcl_app=>set_sel_screen_functions( ).
*--------------------------------------------------------------------*
* selection screen events
*--------------------------------------------------------------------*
at selection-screen output.
  lcl_app=>screen_modif( ).

at selection-screen on value-request for p_file.
  lcl_app=>f4_file( ).

at selection-screen.
  lcl_app=>input_fld_check( ).
  lcl_app=>process_ucomm( ).
*--------------------------------------------------------------------*
* start-of-selection
*--------------------------------------------------------------------*
start-of-selection.
  lcl_main=>start( ).
*--------------------------------------------------------------------*
* end-of-selection
*--------------------------------------------------------------------*
end-of-selection.
  lcl_app=>set_sel_screen_functions( ).
