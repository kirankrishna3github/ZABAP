*&---------------------------------------------------------------------*
*& Include          ZDM_INWARD_GENERATOR_MAIN
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&  Include           ZGSP_INWARD_GENERATOR_MAIN
*&---------------------------------------------------------------------*
START-OF-SELECTION.
*IF P_STAGE = 'X'.

****  Get Data / Fill table (Done By Cygnet)
  PERFORM GET_DATA.
  PERFORM SET_DATA.
*ENDIF.


  IF P_RAD1 = 'X'.
****  Dispaly data in tabular format (Done By Cygnet)
     PERFORM fieldcatalog.
    PERFORM DISPLAY_DATA.
*ELSEIF p_rad2 = 'X'.
*  PERFORM update_stag_table.

  ELSEIF P_RAD3 = 'X'.
*****  Generate CSV file (Done By Cygnet)
    PERFORM FILE_DOWNLOAD.
    CASE G_VAR_RESPONSE.
      WHEN 0.
        WRITE: / 'Output file  Successfully  Generated For GL'.
      WHEN 1.
        WRITE: / 'Error In FTP Connection During GL File Generation'.
      WHEN 2.
        WRITE: / 'Error In FTP Command During GL  Generation'.
      WHEN 3.
        WRITE: / 'Record Not Found During GL Generation'.
      WHEN 4.
        WRITE: / 'Record Not Found During GL Generation'.
      WHEN 5.
        WRITE: / 'Error During File Generation'.
    ENDCASE.
  ENDIF.

* FORM layout .
*
*  gs_str_layout-colwidth_optimize = 'X'.
*ENDFORM.
*&---------------------------------------------------------------------*
*& Form FIELDCATALOG
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM fieldcatalog .
   wa_fcat-col_pos = 1.
  wa_fcat-tabname = 'L_TAB_OUT_FINAL_API' .
  wa_fcat-fieldname = 'LOCATION_GSTIN' .
  wa_fcat-seltext_m = 'LocationGSTIN' .
  APPEND wa_fcat TO it_fcat.
  CLEAR wa_fcat.

  wa_fcat-col_pos = 2.
  wa_fcat-tabname = 'L_TAB_OUT_FINAL_API' .
  wa_fcat-fieldname = 'LOCATION_LEGAL_NAME' .
  wa_fcat-seltext_m = 'LocationLegalName' .
  APPEND wa_fcat TO it_fcat.
  CLEAR wa_fcat.

*  wa_fcat-col_pos = 3.
*  wa_fcat-tabname = 'L_TAB_OUT_FINAL_API' .
*  wa_fcat-fieldname = 'RETURNPERIOD' .
*  wa_fcat-seltext_m = 'Returnperiod' .
*  APPEND wa_fcat TO it_fcat.
*  CLEAR wa_fcat.
  wa_fcat-col_pos = 3.
  wa_fcat-tabname = 'L_TAB_OUT_FINAL_API' .
  wa_fcat-fieldname = 'DOCUMENT_PURPOSE' .
  wa_fcat-seltext_m = 'Document Purpose' .
  APPEND wa_fcat TO it_fcat.
  CLEAR wa_fcat.

  wa_fcat-col_pos = 4.
  wa_fcat-tabname = 'L_TAB_OUT_FINAL_API' .
  wa_fcat-fieldname = 'SUPPLY_TYPE' .
  wa_fcat-seltext_m = 'Supply Type' .
  APPEND wa_fcat TO it_fcat .
  CLEAR wa_fcat.

  wa_fcat-col_pos = 5.
  wa_fcat-tabname = 'L_TAB_OUT_FINAL_API' .
  wa_fcat-fieldname = 'SUB_SUPPLY_TYPE' .
  wa_fcat-seltext_m = 'Sub Supply Type' .
  APPEND wa_fcat TO it_fcat .
  CLEAR wa_fcat.

  wa_fcat-col_pos = 6.
  wa_fcat-tabname = 'L_TAB_OUT_FINAL_API' .
  wa_fcat-fieldname = 'SUB_SUPPLY_DESCRIPTION' .
  wa_fcat-seltext_m = 'Sub Supply Description'.

  APPEND wa_fcat TO it_fcat .
  CLEAR wa_fcat.

  wa_fcat-col_pos = 7.
  wa_fcat-tabname = 'L_TAB_OUT_FINAL_API' .
  wa_fcat-fieldname = 'DOCUMENT_TYPE' .
  wa_fcat-seltext_m = 'DocumentType' .
  APPEND wa_fcat TO it_fcat .
  CLEAR wa_fcat.

   wa_fcat-col_pos = 8.
  wa_fcat-tabname = 'L_TAB_OUT_FINAL_API' .
  wa_fcat-fieldname = 'DOCUMENT_NO' .
  wa_fcat-seltext_m = 'Document Number'.
  APPEND wa_fcat TO it_fcat .
  CLEAR wa_fcat.

    wa_fcat-col_pos = 9.
  wa_fcat-tabname = 'L_TAB_OUT_FINAL_API' .
  wa_fcat-fieldname = 'SERIES_CODE' .
  wa_fcat-seltext_m = 'Series Code'.
  APPEND wa_fcat TO it_fcat .
  CLEAR wa_fcat.

  wa_fcat-col_pos = 10.
  wa_fcat-tabname = 'L_TAB_OUT_FINAL_API' .
  wa_fcat-fieldname = 'DOCUMENT_DATE' .
  wa_fcat-seltext_m = 'Document Date'.
  APPEND wa_fcat TO it_fcat .
  CLEAR wa_fcat.


      wa_fcat-col_pos = 11.
  wa_fcat-tabname = 'L_TAB_OUT_FINAL_API' .
  wa_fcat-fieldname = 'REFERENCE_DOC_TYPE' .
  wa_fcat-seltext_m = 'Reference Document Type' .
  APPEND wa_fcat TO it_fcat.
  CLEAR wa_fcat.

  wa_fcat-col_pos = 12.
  wa_fcat-tabname = 'L_TAB_OUT_FINAL_API' .
  wa_fcat-fieldname = 'REFERENCE_DOC_NO' .
  wa_fcat-seltext_m = 'Reference Document Number' .
  APPEND wa_fcat TO it_fcat.
  CLEAR wa_fcat.



  wa_fcat-col_pos = 13.
  wa_fcat-tabname = 'L_TAB_OUT_FINAL_API' .
  wa_fcat-fieldname = 'REFERENCE_DOC_DATE' .
  wa_fcat-seltext_m = 'Reference Document Date' .
  APPEND wa_fcat TO it_fcat.
  CLEAR wa_fcat.

  wa_fcat-col_pos = 14.
  wa_fcat-tabname = 'L_TAB_OUT_FINAL_API' .
  wa_fcat-fieldname = 'TRANSACTION_TYPE'.
 wa_fcat-seltext_m = 'Transaction Type' .
  APPEND wa_fcat TO it_fcat .
  CLEAR wa_fcat.

  wa_fcat-col_pos = 15.
  wa_fcat-tabname = 'L_TAB_OUT_FINAL_API' .
  wa_fcat-fieldname = 'POS' .
  wa_fcat-seltext_m = 'Place of Supply' .
  APPEND wa_fcat TO it_fcat .
  CLEAR wa_fcat.

  wa_fcat-col_pos = 16.
  wa_fcat-tabname = 'L_TAB_OUT_FINAL_API' .
  wa_fcat-fieldname = 'BILL_FROM_GSTIN' .
  wa_fcat-seltext_m = 'Bill From: GSTIN'.

  APPEND wa_fcat TO it_fcat .
  CLEAR wa_fcat.

  wa_fcat-col_pos = 17.
  wa_fcat-tabname = 'L_TAB_OUT_FINAL_API' .
  wa_fcat-fieldname = 'BILL_FROM_OTH_PARTY_NAME' .
  wa_fcat-seltext_m = 'Bill From: Other party Name' .
  APPEND wa_fcat TO it_fcat .
  CLEAR wa_fcat.

   wa_fcat-col_pos = 18.
  wa_fcat-tabname = 'L_TAB_OUT_FINAL_API' .
  wa_fcat-fieldname = 'BILL_FROM_ADDRESS1' .
  wa_fcat-seltext_m = 'Bill From: Address Line 1'.
  APPEND wa_fcat TO it_fcat .
  CLEAR wa_fcat.

    wa_fcat-col_pos = 19.
  wa_fcat-tabname = 'L_TAB_OUT_FINAL_API' .
  wa_fcat-fieldname = 'BILL_FROM_ADDRESS2' .
  wa_fcat-seltext_m = 'Bill From: Address Line 1'.
  APPEND wa_fcat TO it_fcat .
  CLEAR wa_fcat.

  wa_fcat-col_pos = 20.
  wa_fcat-tabname = 'L_TAB_OUT_FINAL_API' .
  wa_fcat-fieldname = 'BILL_FROM_CITY' .
  wa_fcat-seltext_m = 'Bill From: City'.
  APPEND wa_fcat TO it_fcat .
  CLEAR wa_fcat.

        wa_fcat-col_pos = 21.
  wa_fcat-tabname = 'L_TAB_OUT_FINAL_API' .
  wa_fcat-fieldname = 'BILL_FROM_STATE' .
  wa_fcat-seltext_m = 'Bill From: State' .
  APPEND wa_fcat TO it_fcat.
  CLEAR wa_fcat.

  wa_fcat-col_pos = 22.
  wa_fcat-tabname = 'L_TAB_OUT_FINAL_API' .
  wa_fcat-fieldname = 'BILL_FROM_POST_CODE' .
  wa_fcat-seltext_m = 'Bill From: Postal Code' .
  APPEND wa_fcat TO it_fcat.
  CLEAR wa_fcat.



  wa_fcat-col_pos = 23.
  wa_fcat-tabname = 'L_TAB_OUT_FINAL_API' .
  wa_fcat-fieldname = 'DISPATCH_FROM_GSTIN' .
  wa_fcat-seltext_m =  'Dispatch From: GSTIN' .
  APPEND wa_fcat TO it_fcat.
  CLEAR wa_fcat.

  wa_fcat-col_pos = 24.
  wa_fcat-tabname = 'L_TAB_OUT_FINAL_API' .
  wa_fcat-fieldname = 'DISPATCH_FROM_OTH_PARTY_NAME'.
 wa_fcat-seltext_m = 'Dispatch From: Other party Name' .
  APPEND wa_fcat TO it_fcat .
  CLEAR wa_fcat.

  wa_fcat-col_pos = 25.
  wa_fcat-tabname = 'L_TAB_OUT_FINAL_API' .
  wa_fcat-fieldname = 'DISPATCH_FROM_ADDRESS1' .
  wa_fcat-seltext_m = 'Dispatch From: Address Line 1' .
  APPEND wa_fcat TO it_fcat .
  CLEAR wa_fcat.

  wa_fcat-col_pos = 26.
  wa_fcat-tabname = 'L_TAB_OUT_FINAL_API' .
  wa_fcat-fieldname = 'DISPATCH_FROM_ADDRESS2' .
  wa_fcat-seltext_m = 'Dispatch From: Address Line 2'.

  APPEND wa_fcat TO it_fcat .
  CLEAR wa_fcat.

  wa_fcat-col_pos = 27.
  wa_fcat-tabname = 'L_TAB_OUT_FINAL_API' .
  wa_fcat-fieldname = 'DISPATCH_FROM_CITY' .
  wa_fcat-seltext_m = 'Dispatch From: City' .
  APPEND wa_fcat TO it_fcat .
  CLEAR wa_fcat.

   wa_fcat-col_pos = 28.
  wa_fcat-tabname = 'L_TAB_OUT_FINAL_API' .
  wa_fcat-fieldname = 'DISPATCH_FROM_STATE' .
  wa_fcat-seltext_m = 'Dispatch From: State'.
  APPEND wa_fcat TO it_fcat .
  CLEAR wa_fcat.

    wa_fcat-col_pos = 29.
  wa_fcat-tabname = 'L_TAB_OUT_FINAL_API' .
  wa_fcat-fieldname = 'DISPATCH_FROM_POST_CODE' .
  wa_fcat-seltext_m = 'Dispatch From: Postal Code'.

     wa_fcat-col_pos = 30.
  wa_fcat-tabname = 'L_TAB_OUT_FINAL_API' .
  wa_fcat-fieldname = 'CTIN' .
  wa_fcat-seltext_m = 'CIN'.

  APPEND wa_fcat TO it_fcat .
  CLEAR wa_fcat.

  wa_fcat-col_pos = 31.
  wa_fcat-tabname = 'L_TAB_OUT_FINAL_API' .
  wa_fcat-fieldname = 'EMAIL' .
  wa_fcat-seltext_m = 'Email Address: Multiple'.
  APPEND wa_fcat TO it_fcat .
  CLEAR wa_fcat.

   wa_fcat-col_pos = 32.
  wa_fcat-tabname = 'L_TAB_OUT_FINAL_API' .
  wa_fcat-fieldname = 'MOBILE' .
  wa_fcat-seltext_m = 'Mobile Numbers: Multiple'.

  APPEND wa_fcat TO it_fcat .
  CLEAR wa_fcat.

  wa_fcat-col_pos = 33.
  wa_fcat-tabname = 'L_TAB_OUT_FINAL_API' .
  wa_fcat-fieldname = 'BILL_TO_GSTIN' .
  wa_fcat-seltext_m = 'Bill To: GSTIN'.
  APPEND wa_fcat TO it_fcat .
  CLEAR wa_fcat.

   wa_fcat-col_pos = 34.
  wa_fcat-tabname = 'L_TAB_OUT_FINAL_API' .
  wa_fcat-fieldname = 'BILL_TO_NAME' .
  wa_fcat-seltext_m = 'Bill To: Name'.

  APPEND wa_fcat TO it_fcat .
  CLEAR wa_fcat.

  wa_fcat-col_pos = 35.
  wa_fcat-tabname = 'L_TAB_OUT_FINAL_API' .
  wa_fcat-fieldname = 'BILL_TO_ADDRESS1' .
  wa_fcat-seltext_m = 'Bill To: Address Line 1'.
  APPEND wa_fcat TO it_fcat .
  CLEAR wa_fcat.

   wa_fcat-col_pos = 36.
  wa_fcat-tabname = 'L_TAB_OUT_FINAL_API' .
  wa_fcat-fieldname = 'BILL_TO_ADDRESS2' .
  wa_fcat-seltext_m = 'Bill To: Address Line 2'.

  APPEND wa_fcat TO it_fcat .
  CLEAR wa_fcat.

  wa_fcat-col_pos = 37.
  wa_fcat-tabname = 'L_TAB_OUT_FINAL_API' .
  wa_fcat-fieldname = 'BILL_TO_STATE' .
  wa_fcat-seltext_m = 'Bill To: State'.
  APPEND wa_fcat TO it_fcat .
  CLEAR wa_fcat.

   wa_fcat-col_pos = 38.
  wa_fcat-tabname = 'L_TAB_OUT_FINAL_API' .
  wa_fcat-fieldname = 'BILL_TO_CITY' .
  wa_fcat-seltext_m = 'Bill To: City'.


  APPEND wa_fcat TO it_fcat .
  CLEAR wa_fcat.

  wa_fcat-col_pos = 39.
  wa_fcat-tabname = 'L_TAB_OUT_FINAL_API' .
  wa_fcat-fieldname = 'BILL_TO_POST_CODE' .
  wa_fcat-seltext_m = 'Bill To: Postal Code'.
  APPEND wa_fcat TO it_fcat .
  CLEAR wa_fcat.

   wa_fcat-col_pos = 40.
  wa_fcat-tabname = 'L_TAB_OUT_FINAL_API' .
  wa_fcat-fieldname = 'SHIP_TO_GSTIN' .
  wa_fcat-seltext_m = 'Ship To: GSTIN'.

  APPEND wa_fcat TO it_fcat .
  CLEAR wa_fcat.

  wa_fcat-col_pos = 41.
  wa_fcat-tabname = 'L_TAB_OUT_FINAL_API' .
  wa_fcat-fieldname = 'SHIP_TO_NAME' .
  wa_fcat-seltext_m = 'Ship To: Name'.
  APPEND wa_fcat TO it_fcat .
  CLEAR wa_fcat.

   wa_fcat-col_pos = 42.
  wa_fcat-tabname = 'L_TAB_OUT_FINAL_API' .
  wa_fcat-fieldname = 'SHIP_TO_ADDRESS1' .
  wa_fcat-seltext_m = 'Ship To: Addrress Line 1'.

  APPEND wa_fcat TO it_fcat .
  CLEAR wa_fcat.

  wa_fcat-col_pos = 43.
  wa_fcat-tabname = 'L_TAB_OUT_FINAL_API' .
  wa_fcat-fieldname = 'SHIP_TO_ADDRESS2' .
  wa_fcat-seltext_m = 'Ship To: Address Line 2'.
  APPEND wa_fcat TO it_fcat .
  CLEAR wa_fcat.

   wa_fcat-col_pos = 44.
  wa_fcat-tabname = 'L_TAB_OUT_FINAL_API' .
  wa_fcat-fieldname = 'SHIP_TO_STATE' .
  wa_fcat-seltext_m = 'Ship To: State'.

  APPEND wa_fcat TO it_fcat .
  CLEAR wa_fcat.

  wa_fcat-col_pos = 45.
  wa_fcat-tabname = 'L_TAB_OUT_FINAL_API' .
  wa_fcat-fieldname = 'SHIP_TO_CITY' .
  wa_fcat-seltext_m = 'Ship To: City'.
  APPEND wa_fcat TO it_fcat .
  CLEAR wa_fcat.

   wa_fcat-col_pos = 46.
  wa_fcat-tabname = 'L_TAB_OUT_FINAL_API' .
  wa_fcat-fieldname = 'SHIP_TO_POST_CODE' .
  wa_fcat-seltext_m = 'Ship To: Postal Code'.

  APPEND wa_fcat TO it_fcat .
  CLEAR wa_fcat.

  wa_fcat-col_pos = 47.
  wa_fcat-tabname = 'L_TAB_OUT_FINAL_API' .
  wa_fcat-fieldname = 'DOCUMENT_VALUE' .
  wa_fcat-seltext_m = 'Document Value'.
  APPEND wa_fcat TO it_fcat .
  CLEAR wa_fcat.

   wa_fcat-col_pos = 48.
  wa_fcat-tabname = 'L_TAB_OUT_FINAL_API' .
  wa_fcat-fieldname = 'OUTSTANDING_AMOUNT' .
  wa_fcat-seltext_m = 'Outstanding Amount'.

  APPEND wa_fcat TO it_fcat .
  CLEAR wa_fcat.

  wa_fcat-col_pos = 49.
  wa_fcat-tabname = 'L_TAB_OUT_FINAL_API' .
  wa_fcat-fieldname = 'REFERENCE_NO' .
  wa_fcat-seltext_m = 'Reference No.'.
  APPEND wa_fcat TO it_fcat .
  CLEAR wa_fcat.

   wa_fcat-col_pos = 50.
  wa_fcat-tabname = 'L_TAB_OUT_FINAL_API' .
  wa_fcat-fieldname = 'DIFF_PERCENTAGE' .
  wa_fcat-seltext_m = 'Differential Percentage'.
  APPEND wa_fcat TO it_fcat .
  CLEAR wa_fcat.

   wa_fcat-col_pos = 51.
  wa_fcat-tabname = 'L_TAB_OUT_FINAL_API' .
  wa_fcat-fieldname = 'REV_CHARGES' .
  wa_fcat-seltext_m = 'Indicate is reverse charge applicable? (Y/N)'.

  APPEND wa_fcat TO it_fcat .
  CLEAR wa_fcat.

  wa_fcat-col_pos = 52.
  wa_fcat-tabname = 'L_TAB_OUT_FINAL_API' .
  wa_fcat-fieldname = 'GSTIN_OF_ECOMM' .
  wa_fcat-seltext_m = 'GSTIN of eCommerce.'.
  APPEND wa_fcat TO it_fcat .
  CLEAR wa_fcat.

   wa_fcat-col_pos = 53.
  wa_fcat-tabname = 'L_TAB_OUT_FINAL_API' .
  wa_fcat-fieldname = ' SHIP_BILL_NO' .
  wa_fcat-seltext_m = 'Shipping Or Export Bill Number'.
  APPEND wa_fcat TO it_fcat .
  CLEAR wa_fcat.

   wa_fcat-col_pos = 54.
  wa_fcat-tabname = 'L_TAB_OUT_FINAL_API' .
  wa_fcat-fieldname = 'SHIP_BILL_DATE' .
  wa_fcat-seltext_m = 'Shipping Or Export Bill Date'.

  APPEND wa_fcat TO it_fcat .
  CLEAR wa_fcat.

  wa_fcat-col_pos = 55.
  wa_fcat-tabname = 'L_TAB_OUT_FINAL_API' .
  wa_fcat-fieldname = 'PORT_CODE' .
  wa_fcat-seltext_m = 'Port Code'.
  APPEND wa_fcat TO it_fcat .
  CLEAR wa_fcat.

    wa_fcat-col_pos = 56.
  wa_fcat-tabname = 'L_TAB_OUT_FINAL_API' .
  wa_fcat-fieldname = 'TRANS_ID' .
  wa_fcat-seltext_m = 'Transporter ID'.
  APPEND wa_fcat TO it_fcat .
  CLEAR wa_fcat.

   wa_fcat-col_pos = 57.
  wa_fcat-tabname = 'L_TAB_OUT_FINAL_API' .
  wa_fcat-fieldname = 'TRANS_NAME' .
  wa_fcat-seltext_m = 'Transporter Name'.

  APPEND wa_fcat TO it_fcat .
  CLEAR wa_fcat.

  wa_fcat-col_pos = 58.
  wa_fcat-tabname = 'L_TAB_OUT_FINAL_API' .
  wa_fcat-fieldname = 'TRANS_DATE' .
  wa_fcat-seltext_m = 'Transport Date'.
  APPEND wa_fcat TO it_fcat .
  CLEAR wa_fcat.

   wa_fcat-col_pos = 59.
  wa_fcat-tabname = 'L_TAB_OUT_FINAL_API' .
  wa_fcat-fieldname = 'TRANS_TIME' .
  wa_fcat-seltext_m = 'Transport Time'.
  APPEND wa_fcat TO it_fcat .
  CLEAR wa_fcat.

   wa_fcat-col_pos = 60.
  wa_fcat-tabname = 'L_TAB_OUT_FINAL_API' .
  wa_fcat-fieldname = 'DISTANCE' .
  wa_fcat-seltext_m = 'Distance Level (Km)'.

  APPEND wa_fcat TO it_fcat .
  CLEAR wa_fcat.

  wa_fcat-col_pos = 61.
  wa_fcat-tabname = 'L_TAB_OUT_FINAL_API' .
  wa_fcat-fieldname = 'TRANS_MODE' .
  wa_fcat-seltext_m = 'Transport Mode'.
  APPEND wa_fcat TO it_fcat .
  CLEAR wa_fcat.

  wa_fcat-col_pos = 62.
  wa_fcat-tabname = 'L_TAB_OUT_FINAL_API' .
  wa_fcat-fieldname = 'TRANS_DOC_NO' .
  wa_fcat-seltext_m = 'Transport Document Number'.
  APPEND wa_fcat TO it_fcat .
  CLEAR wa_fcat.

  wa_fcat-col_pos = 63.
  wa_fcat-tabname = 'L_TAB_OUT_FINAL_API' .
  wa_fcat-fieldname = 'TRANS_DOC_DATE' .
  wa_fcat-seltext_m = 'Transport Document Date'.
  APPEND wa_fcat TO it_fcat .
  CLEAR wa_fcat.

  wa_fcat-col_pos = 64.
  wa_fcat-tabname = 'L_TAB_OUT_FINAL_API' .
  wa_fcat-fieldname = 'VEHICAL_NO' .
  wa_fcat-seltext_m = 'Vehicle Number'.
  APPEND wa_fcat TO it_fcat .
  CLEAR wa_fcat.

  wa_fcat-col_pos = 65.
  wa_fcat-tabname = 'L_TAB_OUT_FINAL_API' .
  wa_fcat-fieldname = 'VEHICAL_TYPE' .
  wa_fcat-seltext_m = 'Vehicle Type'.
  APPEND wa_fcat TO it_fcat .
  CLEAR wa_fcat.

   wa_fcat-col_pos = 66.
  wa_fcat-tabname = 'L_TAB_OUT_FINAL_API' .
  wa_fcat-fieldname = 'ORIG_DOC_NO' .
  wa_fcat-seltext_m = 'Original Document Number'.
  APPEND wa_fcat TO it_fcat .
  CLEAR wa_fcat.

  wa_fcat-col_pos = 67.
  wa_fcat-tabname = 'L_TAB_OUT_FINAL_API' .
  wa_fcat-fieldname = 'ORIG_DOC_DATE' .
  wa_fcat-seltext_m = 'Original Document Date'.
  APPEND wa_fcat TO it_fcat .
  CLEAR wa_fcat.

  wa_fcat-col_pos = 68.
  wa_fcat-tabname = 'L_TAB_OUT_FINAL_API' .
  wa_fcat-fieldname = 'CUSTOM1' .
  wa_fcat-seltext_m = 'Custom1'.
  APPEND wa_fcat TO it_fcat .
  CLEAR wa_fcat.

  wa_fcat-col_pos = 69.
  wa_fcat-tabname = 'L_TAB_OUT_FINAL_API' .
  wa_fcat-fieldname = 'CUSTOM2' .
  wa_fcat-seltext_m = 'Custom2'.
  APPEND wa_fcat TO it_fcat .
  CLEAR wa_fcat.

  wa_fcat-col_pos = 70.
  wa_fcat-tabname = 'L_TAB_OUT_FINAL_API' .
  wa_fcat-fieldname = 'CUSTOM3' .
  wa_fcat-seltext_m = 'Custom3'.
  APPEND wa_fcat TO it_fcat .
  CLEAR wa_fcat.
   wa_fcat-col_pos = 71.
  wa_fcat-tabname = 'L_TAB_OUT_FINAL_API' .
  wa_fcat-fieldname = 'CUSTOM4' .
  wa_fcat-seltext_m = 'Custom4'.
  APPEND wa_fcat TO it_fcat .
  CLEAR wa_fcat.

  wa_fcat-col_pos = 72.
  wa_fcat-tabname = 'L_TAB_OUT_FINAL_API' .
  wa_fcat-fieldname = 'CUSTOM5' .
  wa_fcat-seltext_m = 'Custom5'.
  APPEND wa_fcat TO it_fcat .
  CLEAR wa_fcat.

  wa_fcat-col_pos = 73.
  wa_fcat-tabname = 'L_TAB_OUT_FINAL_API' .
  wa_fcat-fieldname = 'DOC_STATUS' .
  wa_fcat-seltext_m = 'Status'.
  APPEND wa_fcat TO it_fcat .
  CLEAR wa_fcat.

    wa_fcat-col_pos = 74.
  wa_fcat-tabname = 'L_TAB_OUT_FINAL_API' .
  wa_fcat-fieldname = 'PRODUCT_NAME' .
  wa_fcat-seltext_m = 'Product Name'.
  APPEND wa_fcat TO it_fcat .
  CLEAR wa_fcat.

    wa_fcat-col_pos = 75.
  wa_fcat-tabname = 'L_TAB_OUT_FINAL_API' .
  wa_fcat-fieldname = 'DESCRIPTION' .
  wa_fcat-seltext_m = 'Description'.
  APPEND wa_fcat TO it_fcat .
  CLEAR wa_fcat.

    wa_fcat-col_pos = 76.
  wa_fcat-tabname = 'L_TAB_OUT_FINAL_API' .
  wa_fcat-fieldname = 'HSN' .
  wa_fcat-seltext_m = 'HSN/SAC'.
  APPEND wa_fcat TO it_fcat .
  CLEAR wa_fcat.

    wa_fcat-col_pos = 77.
  wa_fcat-tabname = 'L_TAB_OUT_FINAL_API' .
  wa_fcat-fieldname = 'QUANTITY' .
  wa_fcat-seltext_m = 'Quantity'.
  APPEND wa_fcat TO it_fcat .
  CLEAR wa_fcat.

    wa_fcat-col_pos = 78.
  wa_fcat-tabname = 'L_TAB_OUT_FINAL_API' .
  wa_fcat-fieldname = 'UOM' .
  wa_fcat-seltext_m = 'Unit of Measure'.
  APPEND wa_fcat TO it_fcat .
  CLEAR wa_fcat.

    wa_fcat-col_pos = 79.
  wa_fcat-tabname = 'L_TAB_OUT_FINAL_API' .
  wa_fcat-fieldname = 'TAX_VALUE' .
  wa_fcat-seltext_m = 'Taxable Value'.
  APPEND wa_fcat TO it_fcat .
   CLEAR wa_fcat.


  wa_fcat-col_pos = 80.
  wa_fcat-tabname = 'L_TAB_OUT_FINAL_API' .
  wa_fcat-fieldname = 'DISCOUNT_AMOUNT' .
  wa_fcat-seltext_m = 'Discount Amount'.

  APPEND wa_fcat TO it_fcat .
  CLEAR wa_fcat.

    wa_fcat-col_pos = 81.
  wa_fcat-tabname = 'L_TAB_OUT_FINAL_API' .
  wa_fcat-fieldname = 'IGST_RATE' .
  wa_fcat-seltext_m = 'Integrated Tax Rate'.
  APPEND wa_fcat TO it_fcat .
  CLEAR wa_fcat.

    wa_fcat-col_pos = 82.
  wa_fcat-tabname = 'L_TAB_OUT_FINAL_API' .
  wa_fcat-fieldname = 'IGST_AMOUNT' .
  wa_fcat-seltext_m = 'Integrated Tax Amount'.
  APPEND wa_fcat TO it_fcat .
  CLEAR wa_fcat.

    wa_fcat-col_pos = 83.
  wa_fcat-tabname = 'L_TAB_OUT_FINAL_API' .
  wa_fcat-fieldname = 'CGST_RATE' .
  wa_fcat-seltext_m = 'Central Tax Rate'.
  APPEND wa_fcat TO it_fcat .
  CLEAR wa_fcat.

    wa_fcat-col_pos = 84.
  wa_fcat-tabname = 'L_TAB_OUT_FINAL_API' .
  wa_fcat-fieldname = 'CGST_AMOUNT' .
  wa_fcat-seltext_m = 'Central Tax Amount'.
  APPEND wa_fcat TO it_fcat .
  CLEAR wa_fcat.

    wa_fcat-col_pos = 85.
  wa_fcat-tabname = 'L_TAB_OUT_FINAL_API' .
  wa_fcat-fieldname = 'SGST_RATE' .
  wa_fcat-seltext_m = 'State/UT Tax Rate'.
  APPEND wa_fcat TO it_fcat .
  CLEAR wa_fcat.

    wa_fcat-col_pos = 86.
  wa_fcat-tabname = 'L_TAB_OUT_FINAL_API' .
  wa_fcat-fieldname = 'SGST_AMOUNT' .
  wa_fcat-seltext_m = 'State/UT Tax Amount'.
  APPEND wa_fcat TO it_fcat .
  CLEAR wa_fcat.

    wa_fcat-col_pos = 87.
  wa_fcat-tabname = 'L_TAB_OUT_FINAL_API' .
  wa_fcat-fieldname = 'CESS_RATE' .
  wa_fcat-seltext_m = 'Cess Rate'.
  APPEND wa_fcat TO it_fcat .
   CLEAR wa_fcat.


  wa_fcat-col_pos = 88.
  wa_fcat-tabname = 'L_TAB_OUT_FINAL_API' .
  wa_fcat-fieldname = 'CESS_AMT' .
  wa_fcat-seltext_m = 'Cess Amount'.
  APPEND wa_fcat TO it_fcat .
  CLEAR wa_fcat.

   wa_fcat-col_pos = 89.
  wa_fcat-tabname = 'L_TAB_OUT_FINAL_API' .
  wa_fcat-fieldname = 'CESS_NON_ADVAL_RATE' .
  wa_fcat-seltext_m = 'Cess Non Advalorem Rate'.
  APPEND wa_fcat TO it_fcat .
  CLEAR wa_fcat.

   wa_fcat-col_pos = 90.
  wa_fcat-tabname = 'L_TAB_OUT_FINAL_API' .
  wa_fcat-fieldname = 'CESS_NON_ADVAL_AMT' .
  wa_fcat-seltext_m = 'Cess Non Advalorem Amount'.
  APPEND wa_fcat TO it_fcat .
  CLEAR wa_fcat.

  wa_fcat-col_pos = 91.
  wa_fcat-tabname = 'L_TAB_OUT_FINAL_API' .
  wa_fcat-fieldname = 'CESS_NON_ADVAL_AMT' .
  wa_fcat-seltext_m = 'Other Charges'.
  APPEND wa_fcat TO it_fcat .
  CLEAR wa_fcat.


   wa_fcat-col_pos = 92.
  wa_fcat-tabname = 'L_TAB_OUT_FINAL_API' .
  wa_fcat-fieldname = 'ELIG_ITC' .
  wa_fcat-seltext_m = 'Eligibility'.
  APPEND wa_fcat TO it_fcat .
  CLEAR wa_fcat.
   wa_fcat-col_pos = 93.
  wa_fcat-tabname = 'L_TAB_OUT_FINAL_API' .
  wa_fcat-fieldname = 'INTR_ITC_AMT' .
  wa_fcat-seltext_m = 'Integrated Tax ITC Amount'.
  APPEND wa_fcat TO it_fcat .
   CLEAR wa_fcat.


  wa_fcat-col_pos = 94.
  wa_fcat-tabname = 'L_TAB_OUT_FINAL_API' .
  wa_fcat-fieldname = 'CENT_ITC_AMT' .
  wa_fcat-seltext_m = 'Central Tax ITC Amount'.
  APPEND wa_fcat TO it_fcat .
  CLEAR wa_fcat.

   wa_fcat-col_pos = 95.
  wa_fcat-tabname = 'L_TAB_OUT_FINAL_API' .
  wa_fcat-fieldname = 'UT_ITC_AMT' .
  wa_fcat-seltext_m = 'State/UT Tax ITC Amount'.
  APPEND wa_fcat TO it_fcat .
  CLEAR wa_fcat.

   wa_fcat-col_pos = 96.
  wa_fcat-tabname = 'L_TAB_OUT_FINAL_API' .
  wa_fcat-fieldname = 'CESS_ITC_AMT' .
  wa_fcat-seltext_m = 'Cess ITC Amount'.
  APPEND wa_fcat TO it_fcat .
  CLEAR wa_fcat.

    wa_fcat-col_pos = 97.
  wa_fcat-tabname = 'L_TAB_OUT_FINAL_API' .
  wa_fcat-fieldname = 'CUSTOM6' .
  wa_fcat-seltext_m = 'Custom6'.
  APPEND wa_fcat TO it_fcat .
  CLEAR wa_fcat.

  wa_fcat-col_pos = 98.
  wa_fcat-tabname = 'L_TAB_OUT_FINAL_API' .
  wa_fcat-fieldname = 'CUSTOM7' .
  wa_fcat-seltext_m = 'Custom7'.
  APPEND wa_fcat TO it_fcat .
  CLEAR wa_fcat.

  wa_fcat-col_pos = 99.
  wa_fcat-tabname = 'L_TAB_OUT_FINAL_API' .
  wa_fcat-fieldname = 'CUSTOM8' .
  wa_fcat-seltext_m = 'Custom8'.
  APPEND wa_fcat TO it_fcat .
  CLEAR wa_fcat.
   wa_fcat-col_pos = 100.
  wa_fcat-tabname = 'L_TAB_OUT_FINAL_API' .
  wa_fcat-fieldname = 'CUSTOM9' .
  wa_fcat-seltext_m = 'Custom9'.
  APPEND wa_fcat TO it_fcat .
  CLEAR wa_fcat.

  wa_fcat-col_pos =  101.
  wa_fcat-tabname = 'L_TAB_OUT_FINAL_API' .
  wa_fcat-fieldname = 'CUSTOM10' .
  wa_fcat-seltext_m = 'Custom10'.
  APPEND wa_fcat TO it_fcat .
  CLEAR wa_fcat.


   wa_fcat-col_pos = 102.
  wa_fcat-tabname = 'L_TAB_OUT_FINAL_API' .
  wa_fcat-fieldname = 'BUKRS' .
  wa_fcat-seltext_m = 'Company Code'.
  APPEND wa_fcat TO it_fcat .
  CLEAR wa_fcat.
*ENDFORM.

ENDFORM.
