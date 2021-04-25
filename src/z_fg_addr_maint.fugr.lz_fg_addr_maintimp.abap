class lcl_app implementation.
  method restrict_sel_opt.
    call function 'SELECT_OPTIONS_RESTRICT'
      exporting
        restriction            = value sscr_restrict(
                                   opt_list_tab = value #(
                                     ( name = 'I_EQ_ONLY'          " Description of restriction
                                       options-eq = abap_true ) )  " option = 'EQ' only
                                   ass_tab = value #(
                                     ( kind = 'S'                  " field type = select option
                                       name = 'S_DELETE'           " name of select option to restrict
                                       sg_main = 'I'               " sign = 'I'
                                       op_main = 'I_EQ_ONLY' ) ) ) " Description of restriction
      exceptions
        too_late               = 1           " Call is too late
        repeated               = 2           " Multiple call using LDB or report
        selopt_without_options = 3           " One of the select-options contains no valid options
        selopt_without_signs   = 4           " One of the select-options does not have a valid sign
        invalid_sign           = 5           " Invalid sign
        empty_option_list      = 6           " One of the options lists is empty
        invalid_kind           = 7           " One line has a KIND value unequal to A, B, or S
        repeated_kind_a        = 8           " More than one line has KIND = 'A'
        others                 = 9.
    if sy-subrc <> 0 and sy-subrc <> 2.
      " function may be called multiple times from the pbo,
      " so the "repeated" exception(subrc = 2) is ok - #warning
      message id sy-msgid type sy-msgty number sy-msgno
        with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    endif.
  endmethod.

  method sel_screen_pbo.
    " Disable ranges and exclusions for s_delete select option
    " For some reason this is not working from the initialization event
    restrict_sel_opt( ).
*--------------------------------------------------------------------*
    " Hide/show selection screen parameters based on
    " the processing mode/selected action
    loop at screen.
      screen-active = '1'.
      if r_single = abap_true.
        if screen-group1 = 'FIL'.
          screen-active = '0'.
        endif.
        if r_create = abap_true.
          if screen-group1 = 'UPD' or screen-group1 = 'DEL'.
            screen-active = '0'.
          endif.
        endif.
        if r_update = abap_true.
          if screen-group1 = 'DEL'.
            screen-active = '0'.
          endif.
        endif.
        if r_delete = abap_true.
          if screen-group1 = 'UPD'.
            screen-active = '0'.
          endif.
        endif.
      endif.
      if r_mass = abap_true.
        if screen-group1 = 'UPD'.
          screen-active = '0'.
        endif.
        if r_delete = abap_true.
          if screen-group1 = 'FIL'.
            screen-active = '0'.
          endif.
        else.
          if screen-group1 = 'DEL'.
            screen-active = '0'.
          endif.
        endif.
      endif.
      modify screen.
    endloop.
*--------------------------------------------------------------------*
    " Enable/disable the additional function key based on
    " the processing mode/selected action
    if r_mass = abap_true.
      if r_create = abap_true or r_update = abap_true.
        sscrfields-functxt_01 =
          value smp_dyntxt( icon_id   = icon_xls
                            quickinfo = 'Download File Format'
                            icon_text = 'Download File Format' ).
      else.
        clear sscrfields-functxt_01.
      endif.
    else.
      clear sscrfields-functxt_01.
    endif.
  endmethod.

  method sel_screen_pai.
    " Prevent users from specifying multiple address number
    " when the processing mode is "single deletion"
    " This is a workaround for disabling the extensions button
    if sscrfields-ucomm = '%017' and r_single = abap_true and r_delete = abap_true.
      set cursor field 'S_DELETE-LOW'.
      message 'Only single address specification allowed' type 'E'.
    endif.
*--------------------------------------------------------------------*
    " handle file format download
    if sscrfields-ucomm = 'FC01'.
      lcl_app=>download_file_format( ).
    endif.
*--------------------------------------------------------------------*
    " check mandatory inputs before execution
    if sscrfields-ucomm = 'CRET'.
      if r_single = abap_true.
        if r_update = abap_true and p_update is initial.
          set cursor field 'P_UPDATE'.
          message e055(00).
        endif.
      endif.

      if r_mass = abap_true.
        if r_create = abap_true or r_update = abap_true.
          if p_file is initial.
            set cursor field 'P_FILE'.
            message e055(00).
          endif.
        endif.
      endif.

      if r_delete = abap_true and s_delete[] is initial.
        set cursor field 'S_DELETE-LOW'.
        message e055(00).
      endif.
*--------------------------------------------------------------------*
      " start primary execution
      try.
          new lcl_app( )->process( ).
        catch cx_root into data(lox_root).
          message lox_root->get_text( ) type 'S' display like 'E'.
      endtry.
    endif.
*--------------------------------------------------------------------*
  endmethod.

  method download_file_format.
    " build and download the excel file format for mass address creation & update
    " based on the action selected by the user
    data:
      lt_create_format type standard table of addr1_data,

      begin of ls_update_format,
        addrnumber type adrc-addrnumber.
        include type addr1_data.
    data:
    end of ls_update_format,
    lt_update_format like standard table of ls_update_format.

    append initial line to lt_create_format.
    append initial line to lt_update_format.

    field-symbols <lt_file_format> type standard table.

    unassign <lt_file_format>.

    " cond #( ==> does not work because the '#' always uses the type
    " of the result of the first 'when'. The values of the subsequent
    " when's are either trucated or converted to the type of the result
    " of the first when which may give unexpected results
    case abap_true.
      when r_create.
        assign lt_create_format to <lt_file_format>.
      when r_update.
        assign lt_update_format to <lt_file_format>.
      when others.
    endcase.

    if <lt_file_format> is assigned.
      zcl_helper=>itab_to_excel(
        exporting
          it_itab = <lt_file_format>        " Single internal table to be converted
          iv_file_path =
            zcl_helper=>file_save_dialog(   " Filepath on frontend or app server to download to
              exporting
                iv_file_filter       = cl_gui_frontend_services=>filetype_excel
                iv_default_file_name = conv #( |standalone_address_| &&
                                               |{ cond #( when r_create = abap_true
                                                          then 'create'
                                                          when r_update = abap_true
                                                          then 'update' ) }_| &&
                                               |file_format.xlsx| ) ) ).
    endif.
  endmethod.

  method process.
    case abap_true.
      when r_single.
        case abap_true.
          when r_create.
          when r_update.
          when r_delete.
          when others.
        endcase.
      when r_mass.
        case abap_true.
          when r_create.
          when r_update.
          when r_delete.
          when others.
        endcase.
      when others.
    endcase.
  endmethod.
endclass.

class lcl_main implementation.
  method start.
    " display the selection screen
    call screen 1001.
  endmethod.
endclass.
