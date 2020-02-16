*&---------------------------------------------------------------------*
*& Report Z_PRG_COPY_VARIANTS_LAYOUTS
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
report z_prg_copy_variants_layouts.

* -- global data -- *
data: usrnm   type ltdx-username,
      variant type ltdx-variant.

* -- selection screen -- *
selection-screen begin of block sel with frame title text-sel.
parameters: p_src type repid modif id sel, " obligatory,
            p_tar type repid modif id sel. " obligatory.
selection-screen end of block sel.

selection-screen begin of block fil with frame title text-fil.
parameters: p_ltdx  type ibipparms-path modif id fil,
            p_ltdxt type ibipparms-path modif id fil.
selection-screen begin of line.
selection-screen comment 1(79) text-tip modif id fil.
selection-screen end of line.
selection-screen end of block fil.

selection-screen begin of block flt with frame title text-flt.
select-options: s_usrnm for usrnm modif id flt,
                s_vrnt  for variant modif id flt.
parameters: c_ovrwrt as checkbox modif id flt.
selection-screen end of block flt.

selection-screen begin of block rad with frame title text-rad.
parameters: r_var radiobutton group rad user-command com default 'X',
            r_lay radiobutton group rad,
            r_adj radiobutton group rad,
            r_lup radiobutton group rad.
selection-screen end of block rad.

* -- local class definition -- *
class lcx_generic definition inheriting from cx_static_check.
endclass.

class lcl_application definition final.
  public section.
    methods: process.
    class-methods: screen_modif,
      f4_file
        importing
          field_name type dynpread-fieldname
        changing
          file_name  type ibipparms-path.
  protected section.

  private section.
    data: begin of gs_log.
        include type ltdxkey.
    data: message type bapi_msg,
          end of gs_log,
          gt_log like standard table of gs_log.

    methods: validate_input raising lcx_generic,
      get_layouts
        importing
          iv_report type repid
        changing
          ct_layout type standard table,
      copy_variants,
      copy_layouts,
      upload_layouts.
endclass.

class lcl_main definition.
  public section.
    class-methods: start.

  protected section.

  private section.
endclass.

* -- local class implementation -- *
class lcl_application implementation.
  method screen_modif.
    case abap_true.
      when r_var or r_lay or r_adj.
        loop at screen.
          if screen-group1 = 'FIL'.
            screen-active = 0.
            screen-invisible = 1.
            modify screen.
          endif.
        endloop.
      when r_lup.
        loop at screen.
          if screen-group1 = 'SEL'.
            screen-active = 0.
            screen-invisible = 1.
            modify screen.
          endif.
        endloop.
      when others.
    endcase.

    if r_var eq abap_true.
      loop at screen.
        if screen-group1 eq 'FLT'.
          screen-active = 0.
          screen-invisible = 1.
          modify screen.
        endif.
      endloop.
    endif.
  endmethod.

  method f4_file.

    call function 'F4_FILENAME'
      exporting
        program_name  = syst-cprog
        dynpro_number = syst-dynnr
        field_name    = field_name
      importing
        file_name     = file_name.

  endmethod.

  method process.
    try.
        validate_input( ).
        case abap_true.
          when r_var.
            copy_variants( ).
          when r_lay.
            copy_layouts( ).
          when r_lup.
            upload_layouts( ).
          when others.
        endcase.
      catch lcx_generic.
        return.
    endtry.

    commit work and wait.

    check gt_log is not initial.
    cl_demo_output=>display( gt_log ).
  endmethod.

  method validate_input.
    if ( r_var eq abap_true or r_lay eq abap_true ) and ( p_src is initial or p_tar is initial ).
      message id '00' type 'S' number '055' display like 'E'.
      raise exception type lcx_generic.
    endif.

    if ( r_var eq abap_true or r_lay eq abap_true ).
      select * from trdir
        into table @data(lt_repid)
        where name in ( @p_src, @p_tar ).

      if lines( lt_repid ) ne 2.
        message 'Report not found!' type 'S' display like 'E'.
        raise exception type lcx_generic.
      endif.
    endif.
  endmethod.

  method copy_variants.
    call function 'RS_COPY_SELECTION_SETS'
      exporting
        source_report  = conv rsvar-report( p_src )
        target_report  = conv rsvar-report( p_tar )
        change_ename   = abap_false
      exceptions
        variant_locked = 1
        others         = 2.
    if sy-subrc <> 0.
* Implement suitable error handling here
    else.
      message 'Variants copied!' type 'S'.
    endif.
  endmethod.

  method copy_layouts.
    data: begin of ls_layout_src.
        include type ltdx.
    data: langu type ltdxt-langu,
          text  type ltdxt-text,
          end of ls_layout_src,
          lt_layout_src like standard table of ls_layout_src,
          lt_layout_tar like lt_layout_src.

    refresh: lt_layout_src, lt_layout_tar.
    get_layouts( exporting iv_report = p_src changing ct_layout = lt_layout_src ).
    get_layouts( exporting iv_report = p_tar changing ct_layout = lt_layout_tar ).

    if sy-subrc <> 0.
* Implement suitable error handling here
      message 'No variants read from source' type 'S' display like 'E'.
    elseif lt_layout_src is not initial.
      sort lt_layout_src ascending by relid report handle log_group username variant type srtf2.
      delete adjacent duplicates from lt_layout_src comparing relid report handle log_group username variant type srtf2.
      data is_layout type upd_s_layo.

      refresh gt_log.

      loop at lt_layout_src into ls_layout_src.
        clear: gs_log, is_layout.

        move-corresponding: ls_layout_src to gs_log, ls_layout_src to is_layout.
        is_layout-report = p_tar.
        try.
            data(ls_layout_tar) = lt_layout_tar[ relid     = ls_layout_src-relid
                                                 report    = p_tar
                                                 handle    = ls_layout_src-handle
                                                 log_group = ls_layout_src-log_group
                                                 username  = ls_layout_src-username
                                                 variant   = ls_layout_src-variant
                                                 type      = ls_layout_src-type ].
            if c_ovrwrt eq abap_false.
              gs_log-message = 'Layout already exists. Skipped.'.
              append gs_log to gt_log.
              clear ls_layout_src.
              continue.
            endif.
          catch cx_sy_itab_line_not_found.

        endtry.

        data(lv_ok) = zcl_helper=>update_report_variants_db( exporting is_layout = is_layout ).

        if lv_ok ne abap_true.
* Implement suitable error handling here
          gs_log-message = 'Layout could not be copied'.
        else.
          gs_log-message = 'Layout copied'.
        endif.

        append gs_log to gt_log.
        clear ls_layout_src.
      endloop.
    endif.
  endmethod.

  method get_layouts.
    select a~relid
           a~report
           a~handle
           a~log_group
           a~username
           a~variant
           a~type
           a~srtf2
           a~version
           a~erfdat
           a~erftime
           a~erfname
           a~aedat
           a~aetime
           a~aename
           a~dependvars
           a~inactive
           a~clustr
           a~clustd
           b~langu
           b~text
      from ltdx as a
      inner join ltdxt as b
      on  a~relid     = b~relid
      and a~report    = b~report
      and a~handle    = b~handle
      and a~log_group = b~log_group
      and a~username  = b~username
      and a~variant   = b~variant
      and a~type      = b~type
      into corresponding fields of table ct_layout
      where a~relid  = 'LT'
      and   a~report = iv_report
      and   a~type   = 'F'
      and   a~username in s_usrnm
      and   a~variant  in s_vrnt.
  endmethod.

  method upload_layouts.
    if p_ltdx is not initial and p_ltdxt is not initial.
      data: i_tab_raw_data type truxs_t_text_data.
      types: excelfield type c length 50.
      data: begin of ls_excel_ltdx,
              relid      type excelfield,
              report     type excelfield,
              handle     type excelfield,
              log_group  type excelfield,
              username   type excelfield,
              variant    type excelfield,
              type       type excelfield,
              srtf2      type excelfield,
              version    type excelfield,
              erfdat     type excelfield,
              erftime    type excelfield,
              erfname    type excelfield,
              aedat      type excelfield,
              aetime     type excelfield,
              aename     type excelfield,
              dependvars type excelfield,
              inactive   type excelfield,
              clustr     type excelfield,
              clustd     type string,
            end of ls_excel_ltdx,
            lt_excel_ltdx like standard table of ls_excel_ltdx,

            begin of ls_excel_ltdxt,
              relid     type excelfield,
              report    type excelfield,
              handle    type excelfield,
              log_group type excelfield,
              username  type excelfield,
              variant   type excelfield,
              type      type excelfield,
              langu     type excelfield,
              text      type excelfield,
            end of ls_excel_ltdxt,
            lt_excel_ltdxt like standard table of ls_excel_ltdxt.

      data: begin of ls_layout.
          include type ltdx.
      data: langu type ltdxt-langu,
            text  type ltdxt-text,
            end of ls_layout,
            lt_layout like standard table of ls_layout.

      refresh lt_excel_ltdx.
      call function 'TEXT_CONVERT_XLS_TO_SAP'
        exporting
          i_field_seperator    = abap_true
          i_line_header        = abap_true
          i_tab_raw_data       = i_tab_raw_data
          i_filename           = conv rlgrap-filename( p_ltdx )
        tables
          i_tab_converted_data = lt_excel_ltdx
        exceptions
          conversion_failed    = 1
          others               = 2.
      if sy-subrc <> 0.
* Implement suitable error handling here
        message 'No data uploaded' type 'S' display like 'E'.
      endif.

      refresh lt_excel_ltdxt.
      call function 'TEXT_CONVERT_XLS_TO_SAP'
        exporting
          i_field_seperator    = abap_true
          i_line_header        = abap_true
          i_tab_raw_data       = i_tab_raw_data
          i_filename           = conv rlgrap-filename( p_ltdxt )
        tables
          i_tab_converted_data = lt_excel_ltdxt
        exceptions
          conversion_failed    = 1
          others               = 2.
      if sy-subrc <> 0.
* Implement suitable error handling here
        message 'No data uploaded' type 'S' display like 'E'.
      endif.

      if lt_excel_ltdx is not initial and lt_excel_ltdxt is not initial.
        data: is_layout type upd_s_layo.

        loop at lt_excel_ltdx into ls_excel_ltdx.
          do.
            assign component sy-index of structure ls_excel_ltdx to field-symbol(<fs>).
            if sy-subrc = 0 and <fs> is assigned.
              condense <fs>.
              unassign <fs>.
            else.
              exit.
            endif.
          enddo.
          clear ls_excel_ltdx.
        endloop.

        loop at lt_excel_ltdxt into ls_excel_ltdxt.
          do.
            assign component sy-index of structure ls_excel_ltdxt to <fs>.
            if sy-subrc = 0 and <fs> is assigned.
              condense <fs>.
              unassign <fs>.
            else.
              exit.
            endif.
          enddo.
          clear ls_excel_ltdxt.
        endloop.

        loop at lt_excel_ltdx into ls_excel_ltdx.
          clear: ls_layout, ls_excel_ltdxt.
          move-corresponding ls_excel_ltdx to ls_layout.

          clear: ls_layout-aedat, ls_layout-erfdat, ls_layout-aetime, ls_layout-erftime.

          if ls_excel_ltdx-aedat ca '/'.
            data(splitter) = '/'.
          elseif ls_excel_ltdx-aedat ca '.'.
            splitter = '.'.
          elseif ls_excel_ltdx-aedat ca '-'.
            splitter = '-'.
          endif.

          if ls_excel_ltdx-aedat is not initial.
            split ls_excel_ltdx-aedat at splitter into data(dd) data(mm) data(yyyy).
            ls_layout-aedat = yyyy && mm && dd.
            clear: dd, mm, yyyy.
          endif.

          clear splitter.
          if ls_excel_ltdx-erfdat ca '/'.
            splitter = '/'.
          elseif ls_excel_ltdx-erfdat ca '.'.
            splitter = '.'.
          elseif ls_excel_ltdx-erfdat ca '-'.
            splitter = '-'.
          endif.

          if ls_excel_ltdx-erfdat is not initial.
            split ls_excel_ltdx-erfdat at splitter into dd mm yyyy.
            ls_layout-erfdat = yyyy && mm && dd.
          endif.

          ls_layout-erftime = conv t( conv i( ls_excel_ltdx-erftime * 86400 ) ).
          ls_layout-aetime = conv t( conv i( ls_excel_ltdx-aetime * 86400 ) ).

          try.
              ls_excel_ltdxt = lt_excel_ltdxt[ relid     = ls_excel_ltdx-relid
                                               report    = ls_excel_ltdx-report
                                               handle    = ls_excel_ltdx-handle
                                               log_group = ls_excel_ltdx-log_group
                                               username  = ls_excel_ltdx-username
                                               variant   = ls_excel_ltdx-variant
                                               type      = ls_excel_ltdx-type     ].
              ls_layout-langu = ls_excel_ltdxt-langu.
              ls_layout-text = ls_excel_ltdxt-text.
            catch cx_sy_itab_line_not_found.
          endtry.

          append ls_layout to lt_layout.
          clear: ls_excel_ltdx, dd, mm, yyyy, splitter.
        endloop.

        sort lt_layout ascending by relid report handle log_group username variant type srtf2.
        delete adjacent duplicates from lt_layout comparing relid report handle log_group username variant type srtf2.

        delete lt_layout where username not in s_usrnm or variant not in s_vrnt.

        select *
          from ltdx
          for all entries in @lt_layout
          where relid       = @lt_layout-relid
          and   report      = @lt_layout-report
          and   handle      = @lt_layout-handle
          and   log_group   = @lt_layout-log_group
          and   ( username  = @lt_layout-username and username  in @s_usrnm )
          and   ( variant   = @lt_layout-variant  and variant   in @s_vrnt )
          and   type        = @lt_layout-type
          into table @data(lt_ltdx).

        refresh gt_log.
        loop at lt_layout into ls_layout.
          clear: is_layout, gs_log.
          move-corresponding: ls_layout to is_layout, ls_layout to gs_log.
          try.
              data(ls_ltdx) = lt_ltdx[ relid     = ls_layout-relid
                                       report    = ls_layout-report
                                       handle    = ls_layout-handle
                                       log_group = ls_layout-log_group
                                       username  = ls_layout-username
                                       variant   = ls_layout-variant
                                       type      = ls_layout-type ].

              if c_ovrwrt eq abap_false.
                gs_log-message = 'Layout already exists. Skipped.'.
                append gs_log to gt_log.
                clear ls_layout.
                continue.
              endif.
            catch cx_sy_itab_line_not_found.

          endtry.

          data(lv_ok) = zcl_helper=>update_report_variants_db( exporting is_layout = is_layout ).

          if lv_ok ne abap_true.
* Implement suitable error handling here
            gs_log-message = 'Layout could not be copied'.
          else.
            gs_log-message = 'Layout copied'.
          endif.

          append gs_log to gt_log.
          clear ls_layout.
        endloop.
      else.
        message 'No data uploaded' type 'S' display like 'E'.
      endif.

    else.
      message 'Mandatory file paths not specified' type 'S' display like 'E'.
    endif.
  endmethod.
endclass.

class lcl_main implementation.
  method start.
    data(lo_app) = new lcl_application( ).

    check lo_app is bound.
    lo_app->process( ).
  endmethod.
endclass.

* -- program events -- *

* -- selection screen events -- *
at selection-screen output.
  lcl_application=>screen_modif( ).

at selection-screen on radiobutton group rad.
  if r_adj = abap_true.
    submit z_prg_adjust_variants with s_repid eq p_tar via selection-screen and return.
    clear r_adj. r_var = abap_true. " set default
  endif.

at selection-screen on value-request for p_ltdx.
  lcl_application=>f4_file( exporting field_name = 'P_LTDX' changing file_name = p_ltdx ).

at selection-screen on value-request for p_ltdxt.
  lcl_application=>f4_file( exporting field_name = 'P_LTDXT' changing file_name = p_ltdxt ).

* -- start of selection -- *
start-of-selection.
  lcl_main=>start( ).

* -- end of selection -- *
