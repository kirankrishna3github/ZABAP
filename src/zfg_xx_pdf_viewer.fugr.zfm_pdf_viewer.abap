function zfm_pdf_viewer.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_PDF_BINARY) TYPE  XSTRING
*"     VALUE(IV_POPUP) TYPE  ABAP_BOOL OPTIONAL
*"----------------------------------------------------------------------
  clear gv_pdf.
  if iv_pdf_binary is not initial.
    gv_pdf = iv_pdf_binary.
  endif.

  message |To print: Click on the document and press Ctrl + P| type 'S'.

  if iv_popup = abap_true.
    call screen '0100' starting at 20 1 ending at 160 24.
  else.
    call screen '0100'.
  endif.
endfunction.
