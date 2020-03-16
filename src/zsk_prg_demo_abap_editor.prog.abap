*&---------------------------------------------------------------------*
*& Report ZSK_PRG_DEMO_ABAP_EDITOR
*&---------------------------------------------------------------------*
*& ZSK_EXECUTE | 6010859; SaurabhK | Monday, March 16, 2020 23:15:50
*&---------------------------------------------------------------------*
program zsk_prg_demo_abap_editor.

data: ok_code like sy-ucomm,
      report  type progname value 'Z_PROGRAM'.

class lcl_app definition deferred.
data: go_app type ref to lcl_app.

class lcl_app definition.
  public section.
    types: gty_source_line type c length 255.
    data: go_abap_editor  type ref to cl_gui_abapedit, " Explicit declaration to handle non-class based exceptions
          go_abap_display type ref to cl_gui_abapedit, " Explicit declaration to handle non-class based exceptions
          gt_source       type standard table of gty_source_line,
          gv_visible      like cl_gui_abapedit=>visible_true.

    methods: auth_check, set_status, init_control, exit, execute_online,

      syntax_check
        returning
          value(rv_error) type abap_bool,

      pretty_print
        importing
          value(iv_display) type abap_bool default abap_true
        changing
          value(ct_source)  like gt_source optional,

      display_code, load_src_frm_db, save_src_to_db, save_before_exit.

  private section.
    methods:
      get_source_code
        returning
          value(rt_src) like gt_source.
endclass.

class lcl_app implementation.
  method auth_check.
    " Only users who are allowed to use the ABAP Editor
    call function 'AUTHORITY_CHECK_TCODE'
      exporting
        tcode  = 'SE38'
      exceptions
        ok     = 1
        not_ok = 2
        others = 3.
    if sy-subrc ge 2.
      message 'No authority to use ABAP editor' type 'S' display like 'E'.
      leave program.
    endif.

    " Only users who are allowed to create and run $TMP programs
    if sy-subrc < 2.
      authority-check object 'S_DEVELOP'
        id 'DEVCLASS' field '$TMP'
        id 'OBJTYPE'  field 'PROG'
        id 'OBJNAME'  dummy
        id 'P_GROUP'  dummy
        id 'ACTVT'    field '02'.
      if sy-subrc <>  0.
        message 'No authority to use ABAP editor' type 'S' display like 'E'.
        leave program.
      endif.
    endif.
  endmethod.

  method set_status.
    data: lt_fcode type standard table of sy-ucomm.

    if ok_code eq 'SHOW_CODE' and gv_visible eq cl_gui_abapedit=>visible_false.
      lt_fcode = value #( ( 'EXECUTE' )
                          ( 'DEBUG' )
                          ( 'SYNTAX' )
                          ( 'PRETTY' )
                          ( 'SHOW_CODE' ) ).
    endif.
    set pf-status 'STANDARD' excluding lt_fcode.
    set titlebar 'TITLE'.
  endmethod.

  method init_control.
    data(lo_table_descr) = cast cl_abap_tabledescr( cl_abap_typedescr=>describe_by_data( exporting p_data = gt_source ) ).
    if lo_table_descr is bound.
      data(lo_elem_descr) = cast cl_abap_elemdescr( lo_table_descr->get_table_line_type( ) ).
    endif.

    do 50 times.
      cl_progress_indicator=>progress_indicate(
        exporting
          i_text               = 'Starting ABAP editor...'                " Progress Text (If no message transferred in I_MSG*)
          i_processed          = 50                                       " Number of Objects Already Processed
          i_total              = 100                                      " Total Number of Objects to Be Processed
          i_output_immediately = abap_true ).                             " X = Display Progress Immediately
    enddo.

    free: go_abap_editor.
    go_abap_editor = new #(
                      parent = new cl_gui_custom_container(
                                     parent = cl_gui_custom_container=>default_screen
                                     container_name = 'ABAP_EDITOR' )
                      max_number_chars = lo_elem_descr->output_length ).

    check go_abap_editor is bound.

    gt_source = value #( ( '*---- Insert code here ----*' ) ).
    go_abap_editor->set_text(
      exporting
        table           = gt_source
      exceptions
        error_dp        = 1     " Error while sending R/3 table to TextEdit control!
        error_dp_create = 2     " ERROR_DP_CREATE
        error_code_page = 3     " Code Page Error
        others          = 4 ).
    if sy-subrc <> 0.
      message id sy-msgid type sy-msgty number sy-msgno
        with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    endif.

    gv_visible = cl_gui_abapedit=>visible_true.
  endmethod.

  method exit.
    case ok_code.
      when 'BACK'.
        if gv_visible eq cl_gui_abapedit=>visible_false.
          go_abap_editor->set_visible(
            exporting
              visible           = cl_gui_abapedit=>visible_true " Visible
            exceptions
              cntl_error        = 1       " CNTL_ERROR
              cntl_system_error = 2       " CNTL_SYSTEM_ERROR
              others            = 3 ).
          if sy-subrc <> 0.
            message id sy-msgid type sy-msgty number sy-msgno
              with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
          endif.

          go_abap_display->set_visible(
            exporting
              visible           = cl_gui_abapedit=>visible_false " Visible
            exceptions
              cntl_error        = 1       " CNTL_ERROR
              cntl_system_error = 2       " CNTL_SYSTEM_ERROR
              others            = 3 ).

          gv_visible = cl_gui_abapedit=>visible_true.
        else.
          data(lv_leave) = abap_true.
        endif.
      when others.
        lv_leave = abap_true.
    endcase.

    if lv_leave = abap_true.
      select single @abap_true from zsk_t_source into @data(lv_exists) where prog_name eq @report.
      if lv_exists = abap_true.
        go_app->get_source_code( ).
        select source from zsk_t_source into table @data(lt_source) where prog_name eq @report.
        if gt_source <> lt_source.
          go_app->save_before_exit( ).
        else.
          leave program.  " IHDK905405
        endif.
      else.
        go_app->save_before_exit( ).
      endif.
    endif.
  endmethod.

  method execute_online.
    check go_abap_editor is bound.
    check not syntax_check( ).

    data(lt_source) = get_source_code( ).
    check gt_source is not initial.

    do 50 times.
      cl_progress_indicator=>progress_indicate(
        exporting
          i_text               = 'Executing source code online...'           " Progress Text (If no message transferred in I_MSG*)
          i_processed          = 50                                       " Number of Objects Already Processed
          i_total              = 100                                      " Total Number of Objects to Be Processed
          i_output_immediately = abap_true ).                             " X = Display Progress Immediately
    enddo.

    pretty_print( ).

    pretty_print(
      exporting
        iv_display = abap_false
      changing
        ct_source = lt_source ).

    generate subroutine pool lt_source
      name data(lv_program)
      message data(lv_msg)
      include data(lv_include)
      line data(lv_line)
      word data(lv_word)
      offset data(lv_offset)
      shortdump-id data(lv_sid).

    case sy-subrc.
      when 0.
        data(lv_class) = |\\PROGRAM={ lv_program }\\CLASS=LCL_APP|.
        try.
            call method (lv_class)=>main.
          catch cx_sy_dyn_call_error.
        endtry.
      when 4.
        message lv_msg type 'I'.
      when 8.
        message lv_sid type 'I'.
      when others.
    endcase.
  endmethod.

  method syntax_check.
    check go_abap_editor is bound.
    data: lv_msg type string,
          lv_dir type trdir,
          lv_lin type i,
          lv_wrd type string,
          lv_inc type string,
          lv_off type i,
          lv_mid type trmsg_key.

    clear rv_error.
    data(lt_source) = get_source_code( ).
    check gt_source is not initial.

    do 50 times.
      cl_progress_indicator=>progress_indicate(
        exporting
          i_text               = 'Syntax check...'                " Progress Text (If no message transferred in I_MSG*)
          i_processed          = 50                                       " Number of Objects Already Processed
          i_total              = 100                                      " Total Number of Objects to Be Processed
          i_output_immediately = abap_true ).                             " X = Display Progress Immediately
    enddo.

    clear: lv_msg,
           lv_dir,
           lv_lin,
           lv_wrd,
           lv_inc,
           lv_off,
           lv_mid.

    lv_dir = value #( varcl   = abap_true
                      subc    = '1'
                      rstat   = 'K'
                      rmand   = sy-mandt
                      rload   = sy-langu
                      fixpt   = abap_true
                      uccheck = abap_true ).

    syntax-check for lt_source
      message lv_msg
      line lv_lin
      word lv_wrd
      directory entry lv_dir
      include lv_inc
      offset lv_off
      message-id lv_mid.

    if sy-subrc <> 0.
      rv_error = abap_true.
      message lv_msg type 'I' display like 'E'.

      go_abap_editor->select_range(
        exporting
          from_line              = ( lv_lin - 8 )                " from line
          from_pos               = lv_off + 1                    " from position
          to_line                = ( lv_lin - 8 )                " to line
          to_pos                 = lv_off + 1 + strlen( lv_wrd ) " to position
        exceptions
          error_cntl_call_method = 1         " Error while selecting lines within SourceEdit control!
          others                 = 2 ).
      if sy-subrc <> 0.
        message id sy-msgid type sy-msgty number sy-msgno
          with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      endif.
    else.
      pretty_print( ).
      if ok_code eq 'SYNTAX'.
        message s440(eu) with 'source code.'.
      endif.
    endif.
  endmethod.

  method pretty_print.
    if ct_source is initial.
      get_source_code( ).
      ct_source = gt_source.
    endif.

    check ct_source is not initial.
    if ok_code eq 'PRETTY'.

      do 50 times.
        cl_progress_indicator=>progress_indicate(
          exporting
            i_text               = 'Formatting source text...'              " Progress Text (If no message transferred in I_MSG*)
            i_processed          = 50                                       " Number of Objects Already Processed
            i_total              = 100                                      " Total Number of Objects to Be Processed
            i_output_immediately = abap_true ).                             " X = Display Progress Immediately
      enddo.
    endif.

    call function 'PRETTY_PRINTER'
      exporting
        inctoo             = abap_false
      tables
        ntext              = ct_source
        otext              = ct_source
      exceptions
        enqueue_table_full = 1
        include_enqueued   = 2
        include_readerror  = 3
        include_writeerror = 4
        others             = 5.
    if sy-subrc <> 0.
      message id sy-msgid type sy-msgty number sy-msgno
              with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    else.
      if ok_code eq 'PRETTY'.
        message s523(ed).
      endif.
    endif.

    check iv_display eq abap_true.
    go_abap_editor->set_text(
      exporting
        table           = ct_source
      exceptions
        error_dp        = 1     " Error while sending R/3 table to TextEdit control!
        error_dp_create = 2     " ERROR_DP_CREATE
        error_code_page = 3     " Code Page Error
        others          = 4 ).
    if sy-subrc <> 0.
      message id sy-msgid type sy-msgty number sy-msgno
        with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    endif.
  endmethod.

  method display_code.
    check go_abap_editor is bound.
    check not syntax_check( ).
    data(lt_source) = get_source_code( ).

    pretty_print(
      exporting
        iv_display = abap_false
      changing
        ct_source  = lt_source ).

    go_abap_editor->set_visible(
      exporting
        visible           = cl_gui_abapedit=>visible_false " Visible
      exceptions
        cntl_error        = 1       " CNTL_ERROR
        cntl_system_error = 2       " CNTL_SYSTEM_ERROR
        others            = 3 ).
    if sy-subrc <> 0.
      message id sy-msgid type sy-msgty number sy-msgno
        with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    else.
      gv_visible = cl_gui_abapedit=>visible_false.
    endif.

    do 50 times.
      cl_progress_indicator=>progress_indicate(
        exporting
          i_text               = 'Displaying generated code...'           " Progress Text (If no message transferred in I_MSG*)
          i_processed          = 50                                       " Number of Objects Already Processed
          i_total              = 100                                      " Total Number of Objects to Be Processed
          i_output_immediately = abap_true ).                             " X = Display Progress Immediately
    enddo.

    go_abap_display = new #( parent = cl_gui_custom_container=>default_screen
                             max_number_chars = 255 ).

    check go_abap_display is bound.
    go_abap_display->set_text(
      exporting
        table           = lt_source
      exceptions
        error_dp        = 1     " Error while sending R/3 table to TextEdit control!
        error_dp_create = 2     " ERROR_DP_CREATE
        error_code_page = 3     " Code Page Error
        others          = 4 ).
    if sy-subrc <> 0.
      message id sy-msgid type sy-msgty number sy-msgno
        with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    endif.

    go_abap_display->set_readonly_mode(
      exporting
        readonly_mode          = cl_gui_abapedit=>true " read-only mode; eq 0: OFF ; ne 0: ON
      exceptions
        error_cntl_call_method = 1    " Error while setting read-only mode!
        invalid_parameter      = 2    " INVALID_PARAMETER
        error_dp               = 3    " Error from Data Provider
        others                 = 4 ).
    if sy-subrc <> 0.
      message id sy-msgid type sy-msgty number sy-msgno
        with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    endif.
  endmethod.

  method get_source_code.
    refresh gt_source.
    go_abap_editor->get_text(
      importing
        table                  = gt_source       " document text
      exceptions
        error_dp               = 1
        error_cntl_call_method = 2
        others                 = 3 ).
    if sy-subrc <> 0.
      message id sy-msgid type sy-msgty number sy-msgno
        with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    endif.

    data(lt_source) = gt_source[].
*    delete lt_source where table_line+0(1) eq '*'.
*    if lt_source is initial.
*      message 'No source code entered in the editor!' type 'S' display like 'W'.
*      return.
*    endif.

    if gt_source is not initial.
*      delete gt_source where table_line is initial.

      refresh rt_src.

      rt_src[] = value #( ( |program { to_lower( report ) }.| )
                          ( |class lcl_app definition.| )
                          ( |public section.| )
                          ( |class-methods: main.| )
                          ( |endclass.| )
                          ( |class lcl_app implementation.| )
                          ( |method main.| )
                          ( |* <<<< Begin Custom Code >>>> *| ) ).
      if ok_code eq 'DEBUG'.
        append |break-point.| to rt_src.
      endif.
      append lines of gt_source to rt_src.
      concatenate lines of gt_source into data(ls_source) separated by space.
      if to_upper( ls_source ) cs 'WRITE' and to_upper( ls_source ) ns 'LEAVE TO LIST-PROCESSING'.
        append |leave to list-processing.| to rt_src.
      endif.
      append |* <<<< End Custom Code >>>> *| to rt_src.
      append |endmethod.| to rt_src.
      append |endclass.| to rt_src.
    endif.
  endmethod.

  method save_src_to_db.
    select single @abap_true from zsk_t_source into @data(lv_duplicate) where prog_name eq @report.
    if lv_duplicate eq abap_true.
      data: lv_answer type c length 1.

      clear lv_answer.
      call function 'POPUP_TO_CONFIRM'
        exporting
          titlebar              = 'Overwrite source code?'
          text_question         = |Program { report } already exists. Do you want to overwrite it?|
          icon_button_1         = 'ICON_OKAY'
          icon_button_2         = 'ICON_CANCEL'
          display_cancel_button = abap_false
          popup_type            = 'ICON_MESSAGE_WARNING'
        importing
          answer                = lv_answer
        exceptions
          text_not_found        = 1
          others                = 2.
      if sy-subrc <> 0.
* Implement suitable error handling here
      endif.
      if lv_answer ne '1'.
        message 'Action cancelled' type 'S'.
      endif.
    else.
      lv_answer = '1'.
    endif.

    if lv_answer eq '1'.
      get_source_code( ).

      delete from zsk_t_source where prog_name eq report.
      data: lt_src type standard table of zsk_t_source.

      refresh lt_src.
      lt_src = value #( for ls_source in gt_source index into lv_line_id ( prog_name = report
                                                                           line_id = lv_line_id
                                                                           source = ls_source ) ).

      modify zsk_t_source from table lt_src.
      if sy-dbcnt ge 1.
        commit work and wait.
        message |Program { report } saved| type 'S'.
      endif.
    endif.
  endmethod.

  method load_src_frm_db.
    select single @abap_true from zsk_t_source into @data(lv_exists) where prog_name eq @report.
    if lv_exists eq abap_true.
      data: lv_answer type c length 1.

      clear lv_answer.
      call function 'POPUP_TO_CONFIRM'
        exporting
          titlebar              = 'Load source code from DB?'
          text_question         = |Current source/changes(if any) will be lost if program { report } is loaded. Proceed?|
          icon_button_1         = 'ICON_OKAY'
          icon_button_2         = 'ICON_CANCEL'
          display_cancel_button = abap_false
          popup_type            = 'ICON_MESSAGE_WARNING'
        importing
          answer                = lv_answer
        exceptions
          text_not_found        = 1
          others                = 2.
      if sy-subrc <> 0.
* Implement suitable error handling here
      else.
        if lv_answer ne '1'.
          message 'Action cancelled' type 'S'.
        endif.
      endif.
    else.
      message |No previous/saved version of program { report } found!| type 'I' display like 'S'.
    endif.

    if lv_answer eq '1' and lv_exists eq abap_true.
      select source from zsk_t_source into table @data(lt_source) where prog_name eq @report.

      check lt_source is not initial.
      go_abap_editor->set_text(
        exporting
          table           = lt_source
        exceptions
          error_dp        = 1     " Error while sending R/3 table to TextEdit control!
          error_dp_create = 2     " ERROR_DP_CREATE
          error_code_page = 3     " Code Page Error
          others          = 4 ).
      if sy-subrc <> 0.
        message id sy-msgid type sy-msgty number sy-msgno
          with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      else.
        cl_gui_cfw=>flush( ).
        message |Program { report } loaded| type 'S'.
      endif.
    endif.
  endmethod.

  method save_before_exit.
    data: lv_answer type c length 1.

    clear lv_answer.
    call function 'POPUP_TO_CONFIRM'
      exporting
        titlebar              = 'Save before exit?'
        text_question         = |Program { report } was changed. Save before exiting editor? |
        icon_button_1         = ''
        icon_button_2         = ''
        text_button_1         = 'Yes'
        text_button_2         = 'No'
        display_cancel_button = abap_true
        popup_type            = 'ICON_MESSAGE_WARNING'
      importing
        answer                = lv_answer
      exceptions
        text_not_found        = 1
        others                = 2.
    if sy-subrc <> 0.
* Implement suitable error handling here
    endif.
    " IHDK905672
    case lv_answer.
      when '1'.
        go_app->save_src_to_db( ).
        leave program.
      when '2'.
        leave program.
      when 'A'.
        " do nothing
    endcase.
  endmethod.
endclass.

start-of-selection.
  free go_app.
  go_app = new #( ).
  go_app->auth_check( ).
  call screen 100.

*&---------------------------------------------------------------------*
*& Module PBO OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
module pbo output.
  check go_app is bound.
  go_app->set_status( ).
  if go_app->go_abap_editor is not bound.
    go_app->init_control( ).
  endif.
endmodule.
*&---------------------------------------------------------------------*
*&      Module  PAI  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module pai input.
  case ok_code.
    when 'BACK' or 'EXIT' or 'CANCEL'.
      go_app->exit( ).
    when 'EXECUTE'.
      go_app->execute_online( ).
    when 'DEBUG'.
      go_app->execute_online( ).
    when 'SYNTAX'.
      go_app->syntax_check( ).
    when 'PRETTY'.
      go_app->pretty_print( ).
    when 'SHOW_CODE'.
      go_app->display_code( ).
    when 'SAVE'.
      go_app->save_src_to_db( ).
    when 'LOAD_SRC'.
      go_app->load_src_frm_db( ).
    when others.
  endcase.
  refresh go_app->gt_source.
  clear: ok_code, sy-ucomm.
endmodule.
*&---------------------------------------------------------------------*
*&      Module  REPORT_F4  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module report_f4 input.
  select distinct prog_name from zsk_t_source into table @data(lt_values).

  data: lt_return type table of ddshretval.

  call function 'F4IF_INT_TABLE_VALUE_REQUEST'
    exporting
      retfield        = 'PROG_NAME'
      dynpprog        = sy-cprog
      dynpnr          = sy-dynnr
      dynprofield     = 'REPORT'
      value_org       = 'S'
    tables
      value_tab       = lt_values
      return_tab      = lt_return
    exceptions
      parameter_error = 1
      no_values_found = 2
      others          = 3.

  " IHDK905674
  try.
      if lt_return[ 1 ]-fieldval <> report.
        report = lt_return[ 1 ]-fieldval.

        ok_code = 'LOAD_SRC'.

*        cl_gui_cfw=>set_new_ok_code( exporting new_code = 'LOAD_SRC' importing rc = data(lv_rc) ).

*        call function 'SAPGUI_SET_FUNCTIONCODE'
*          exporting
*            functioncode           = 'LOAD_SRC' " Function code
*          exceptions
*            function_not_supported = 1   " Not supported on this front end platform
*            others                 = 2.
*        if sy-subrc <> 0.
*          message id sy-msgid type sy-msgty number sy-msgno
*            with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*        endif.

        suppress dialog.  " trigger pai
      endif.
    catch cx_sy_itab_line_not_found ##no_handler.
  endtry.
endmodule.
