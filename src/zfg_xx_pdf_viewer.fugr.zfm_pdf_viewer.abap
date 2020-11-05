function zfm_pdf_viewer.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_PDF_BINARY) TYPE  XSTRING
*"     VALUE(IV_POPUP) TYPE  ABAP_BOOL DEFAULT ABAP_FALSE
*"     VALUE(IV_PRINT) TYPE  ABAP_BOOL DEFAULT ABAP_FALSE
*"----------------------------------------------------------------------
  clear gv_pdf.
  if iv_pdf_binary is not initial.
    gv_pdf = iv_pdf_binary.
  endif.

  if iv_print = abap_true.
    /scmtms/cl_ui_dlg_print_pdf=>create_spool_and_print_single(
      exporting
        iv_prndst      = conv #( 'LP01' )       " Spool: Output device
        iv_content     = gv_pdf                 " Attachment Folder: Content
        iv_spool_title = conv #( 'Print PDF' )  " Title of a spool request
      importing
        ev_retcode     = data(lv_retcode) ).    " 2 byte integer (signed)
  endif.

  if iv_popup = abap_true.
    call screen '0100' starting at 20 1 ending at 160 24.
  else.
    call screen '0100'.
  endif.
endfunction.
