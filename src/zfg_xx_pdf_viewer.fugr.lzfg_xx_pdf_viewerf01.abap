*----------------------------------------------------------------------*
***INCLUDE LZFG_XX_PDF_VIEWERF01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form SET_STATUS
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
form set_status.
  set pf-status 'PDF_VIEWER'.
  set titlebar 'PDF_VIEWER'.
endform.

form display_pdf.
  clear go_html_viewer.

  data(lo_html_viewer) =
    new cl_gui_html_viewer( parent = cl_gui_custom_container=>default_screen ).

  if lo_html_viewer is bound.
    go_html_viewer = lo_html_viewer.

    data(lv_alignment) = lo_html_viewer->align_at_left  +
                         lo_html_viewer->align_at_right +
                         lo_html_viewer->align_at_top   +
                         lo_html_viewer->align_at_bottom.

    lo_html_viewer->set_alignment(
      exporting
        alignment         = lv_alignment " Alignment
      exceptions
        cntl_error        = 1         " cntl_error
        cntl_system_error = 2         " cntl_system_error
        others            = 3 ).
    if sy-subrc <> 0.
      message id sy-msgid type sy-msgty number sy-msgno
        with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    endif.

    data(lt_pdf) = cl_bcs_convert=>xstring_to_solix(
                     exporting
                       iv_xstring = gv_pdf ).

    data lv_url type c length 2048.

    lo_html_viewer->load_data(
      exporting
        url                    = 'smart.pdf'              " URL
        type                   = 'application'            " Type of a MIME Object
        subtype                = 'pdf'                    " Subtype of a MIME Object
        size                   = xstrlen( gv_pdf )        " Length of Data
      importing
        assigned_url           = lv_url                   " URL
      changing
        data_table             = lt_pdf                   " data table
      exceptions
        dp_invalid_parameter   = 1                " invalid parameter in a DP call
        dp_error_general       = 2                " gerneral error in a DP call
        cntl_error             = 3                " error
        html_syntax_notcorrect = 4                " HTML data is invalid and check all the tags' syntax
        others                 = 5 ).
    if sy-subrc <> 0.
      message id sy-msgid type sy-msgty number sy-msgno
        with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    endif.

    cl_gui_cfw=>flush(
      exceptions
        cntl_system_error = 1 " cntl_system_error
        cntl_error        = 2 " cntl_error
        others            = 3 ).
    if sy-subrc <> 0.
      message id sy-msgid type sy-msgty number sy-msgno
        with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    endif.

    lo_html_viewer->show_data(
      exporting
        url                    = lv_url   " URL
      exceptions
        cntl_error             = 1     " Error in CFW Call
        cnht_error_not_allowed = 2     " Navigation outside R/3 is not allowed
        cnht_error_parameter   = 3     " Incorrect parameters
        dp_error_general       = 4     " Error in DP FM call
        others                 = 5 ).
    if sy-subrc <> 0.
      message id sy-msgid type sy-msgty number sy-msgno
        with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    endif.

    cl_gui_cfw=>flush(
      exceptions
        cntl_system_error = 1 " cntl_system_error
        cntl_error        = 2 " cntl_error
        others            = 3 ).
    if sy-subrc <> 0.
      message id sy-msgid type sy-msgty number sy-msgno
        with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    endif.

    if gv_print = abap_true.
      wait up to 1 seconds.

      lo_html_viewer->execwb(
        exceptions
          cntl_error = 1              " Control Error
          others     = 2 ).
      if sy-subrc <> 0.
        message id sy-msgid type sy-msgty number sy-msgno
          with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      endif.

      cl_gui_cfw=>flush(
        exceptions
          cntl_system_error = 1 " cntl_system_error
          cntl_error        = 2 " cntl_error
          others            = 3 ).
      if sy-subrc <> 0.
        message id sy-msgid type sy-msgty number sy-msgno
          with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      endif.

      if gv_display = abap_true.
        lo_html_viewer->free(
          exceptions
            cntl_error        = 1 " CNTL_ERROR
            cntl_system_error = 2 " CNTL_SYSTEM_ERROR
            others            = 3 ).
        if sy-subrc <> 0.
          message id sy-msgid type sy-msgty number sy-msgno
            with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        endif.

        cl_gui_cfw=>flush( ).

*        leave to screen 0.
      endif.
    endif.

    if gv_display = abap_true.
      message |To print: Click on the document and press 'Ctrl + P'| type 'S'.
    endif.
  endif.
endform.
*&---------------------------------------------------------------------*
*& Form HANDLER_UCOMM
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
form handler_ucomm.
  if sy-ucomm = 'BACK'
    or sy-ucomm = 'EXIT'
    or sy-ucomm = 'CANC'.

    if go_html_viewer is bound.
      go_html_viewer->free(
        exceptions
          cntl_error        = 1 " CNTL_ERROR
          cntl_system_error = 2 " CNTL_SYSTEM_ERROR
          others            = 3 ).
      if sy-subrc <> 0.
        message id sy-msgid type sy-msgty number sy-msgno
          with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      endif.

      clear go_html_viewer.
    endif.

    clear sy-ucomm.
    leave to screen 0.
  endif.

  clear sy-ucomm.
endform.
