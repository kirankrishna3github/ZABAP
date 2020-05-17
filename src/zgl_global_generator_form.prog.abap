*&---------------------------------------------------------------------*
*&  Include           ZGSP_GLOBAL_GENERATOR_FORM
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_GSTR1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM DISPLAY .
*  PERFORM TOP_OF_PAGE.
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      I_CALLBACK_PROGRAM     = SY-REPID
      I_CALLBACK_TOP_OF_PAGE = 'TOP_OF_PAGE'
*     I_STRUCTURE_NAME       = 'ZDN_GLOBAL'
      IS_LAYOUT              = GS_STR_LAYOUT
      IT_FIELDCAT            = IT_FCAT
    TABLES
      T_OUTTAB               = GT_FINAL_OUTWARD
    EXCEPTIONS
      PROGRAM_ERROR          = 1
      OTHERS                 = 2.
  IF SY-SUBRC <> 0.
  ENDIF.
  CLEAR: LT_HEADER.



*perform local_file_generation.
ENDFORM.                    " DISPLAY_GSTR1
*&---------------------------------------------------------------------*
*&      Form  FILE_GENERATE
*** Generate File with ".CSV" fomate for GSTR1 Filing from live and Staging Data any one.
*** Created By :- Aditya Patel.
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*





*AT SELECTION-SCREEN ON VALUE-REQUEST FOR path .
*  CALL FUNCTION 'F4_FILENAME'
*   EXPORTING
*     PROGRAM_NAME        = SYST-CPROG
*     DYNPRO_NUMBER       = SYST-DYNNR
*     FIELD_NAME          = 'path'
*   IMPORTING
*     FILE_NAME           = path.


*perform local_file_generation.
***  TYPES:
***   BEGIN OF TY_OUT_INVOICE,
***     LOCATION_GSTIN            TYPE CHAR50,
***     LOCATION_LEGAL_NAME       TYPE CHAR50,
***     DOCUMENT_PURPOSE          TYPE CHAR50,
***     SUPPLY_TYPE               TYPE CHAR50,
***     SUB_SUPPLY_TYPE           TYPE CHAR50,
***     SUB_SUPPLY_DESCRIPTION    TYPE CHAR50,
***     DOCUMENT_TYPE             TYPE CHAR50,
***     DOCUMENT_NO               TYPE CHAR50,
***     SERIES_CODE               TYPE CHAR50,
***     DOCUMENT_DATE             TYPE CHAR50,
***     REFERENCE_DOC_TYPE        TYPE CHAR50,
***     REFERENCE_DOC_NO          TYPE CHAR50,
***     REFERENCE_DOC_DATE        TYPE CHAR50,
***     TRANSACTION_TYPE          TYPE CHAR50,
***     POS                       TYPE CHAR50,
***     BILL_FROM_GSTIN           TYPE CHAR50,
***     BILL_FROM_OTH_PARTY_NAME  TYPE CHAR50,
***     BILL_FROM_ADDRESS1        TYPE CHAR50,
***     BILL_FROM_ADDRESS2        TYPE CHAR50,
***     BILL_FROM_CITY            TYPE CHAR50,
***     BILL_FROM_STATE           TYPE CHAR50,
***     BILL_FROM_POST_CODE       TYPE CHAR50,
***     DISPATCH_FROM_GSTIN       TYPE CHAR50,
***     DISPATCH_FROM_OTH_PARTY_NAME TYPE CHAR50,
***     DISPATCH_FROM_ADDRESS1       TYPE CHAR50,
***     DISPATCH_FROM_ADDRESS2       TYPE CHAR50,
***     DISPATCH_FROM_CITY           TYPE CHAR50,
***     DISPATCH_FROM_STATE          TYPE CHAR50,
***     DISPATCH_FROM_POST_CODE      TYPE CHAR50,
***     CTIN                         TYPE CHAR50,
***     EMAIL                        TYPE CHAR50,
***     MOBILE                       TYPE CHAR50,
***     BILL_TO_GSTIN                TYPE CHAR50,
***     BILL_TO_NAME                 TYPE CHAR50,
***     BILL_TO_ADDRESS1             TYPE CHAR50,
***     BILL_TO_ADDRESS2             TYPE CHAR50,
***     BILL_TO_STATE                TYPE CHAR50,
***     BILL_TO_CITY                 TYPE CHAR50,
***     BILL_TO_POST_CODE            TYPE CHAR50,
***     SHIP_TO_GSTIN                TYPE CHAR50,
***     SHIP_TO_NAME                 TYPE CHAR50,
***     SHIP_TO_ADDRESS1             TYPE CHAR50,
***     SHIP_TO_ADDRESS2             TYPE CHAR50,
***     SHIP_TO_STATE                TYPE CHAR50,
***     SHIP_TO_CITY                 TYPE CHAR50,
***     SHIP_TO_POST_CODE            TYPE CHAR50,
***     DOCUMENT_VALUE                TYPE CHAR50,
***     OUTSTANDING_AMOUNT           TYPE CHAR50,
***     REFERENCE_NO                 TYPE CHAR50,
***     DIFF_PERCENTAGE              TYPE CHAR50,
***     REV_CHARGES                  TYPE CHAR50,
***     GSTIN_OF_ECOMM               TYPE CHAR50,
***     SHIP_BILL_NO                 TYPE CHAR50,
***     SHIP_BILL_DATE               TYPE CHAR50,
***     PORT_CODE                    TYPE CHAR50,
***     TRANS_ID                     TYPE CHAR50,
***     TRANS_NAME                   TYPE CHAR50,
***     TRANS_DATE                   TYPE CHAR50,
***     TRANS_TIME                   TYPE CHAR50,
***     DISTANCE                     TYPE CHAR50,
***     TRANS_MODE                   TYPE CHAR50,
***     TRANS_DOC_NO                 TYPE CHAR50,
***     TRANS_DOC_DATE               TYPE CHAR50,
***     VEHICAL_NO                   TYPE CHAR50,
***     VEHICAL_TYPE                 TYPE CHAR50,
***     ORIG_GSTIN                   TYPE CHAR50,
***     ORIG_DOC_NO                  TYPE CHAR50,
***     ORIG_DOC_DATE                TYPE CHAR50,
***     PRODUCT_NAME                 TYPE CHAR50,
***     DESCRIPTION                  TYPE CHAR50,
***     HSN                          TYPE CHAR50,
***     QUANTITY                     TYPE CHAR50,
***     UOM                          TYPE CHAR50,
***     TAX_VALUE                    TYPE CHAR50,
***     DISCOUNT_AMOUNT              TYPE CHAR50,
***     IGST_RATE                    TYPE CHAR50,
***     IGST_AMOUNT                  TYPE CHAR50,
***     CGST_RATE                    TYPE CHAR50,
***     CGST_AMOUNT                  TYPE CHAR50,
***     SGST_RATE                    TYPE CHAR50,
***     SGST_AMOUNT                  TYPE CHAR50,
***     CESS_RATE                    TYPE CHAR50,
***     CESS_AMT                     TYPE CHAR50,
***     CESS_NON_ADVAL_RATE          TYPE CHAR50,
***     CESS_NON_ADVAL_AMT           TYPE CHAR50,
***     OTHER_CHARGES                TYPE CHAR50,
***     ELIG_ITC                     TYPE CHAR50,
***     INTR_ITC_AMT                 TYPE CHAR50,
***     CENT_ITC_AMT                 TYPE CHAR50,
***     UT_ITC_AMT                   TYPE CHAR50,
***     CESS_ITC_AMT                 TYPE CHAR50,
***     CUSTOM1                      TYPE CHAR50,
***     CUSTOM2                      TYPE CHAR50,
***     CUSTOM3                      TYPE CHAR50,
***     CUSTOM4                      TYPE CHAR50,
***     CUSTOM5                      TYPE CHAR50,
***     CUSTOM6                      TYPE CHAR50,
***     CUSTOM7                      TYPE CHAR50,
***     CUSTOM8                      TYPE CHAR50,
***     CUSTOM9                      TYPE CHAR50,
***     CUSTOM10                     TYPE CHAR50,
***     WERKS                        TYPE WERKS_D,
***     BUKRS                        TYPE BUKRS,
***     DOC_STATUS                   TYPE CHAR40,
***   END OF TY_OUT_INVOICE.
***Internal Tables******************************************************
***  DATA : LT_FINAL           TYPE STANDARD TABLE OF TY_OUT_INVOICE,
***         LT_DATA            TYPE TABLE OF TEXT,
***         LT_OUTWARD_INVOICE TYPE TRUXS_T_TEXT_DATA,
***         LT_CUST_DATA       TYPE STANDARD TABLE OF ZDM_CUST_DATA.
***Structure***********************************************************
***  DATA : LS_OUT_INVOICE TYPE TY_FINAL_OUTWARD,
***         LS_FINAL       TYPE TY_OUT_INVOICE,
***         LS_DATE       TYPE RANGE_S_DATS,
***         LS_CUST_DATA   TYPE ZDM_CUST_DATA.
***Variables************************************************************
***  DATA : LV_PWD_LEN    TYPE I, "For finding the length of the Password, This is used when scrambling the password
***         LV_HANDLE     TYPE I, "Handle for Pointing to an already connected FTP connection,used for subsequent actions on the connected FTP session
***         LV_PATH       TYPE STRING,"Path that points to the FTP User's Home Directory
***         LV_TSL        TYPE TIMESTAMPL, "Time Stamp
***         LV_TIME_STAMP TYPE CHAR21, "Time Stamp
***         LV_BUKRS      TYPE BUKRS,
***         LV_ANSWER     TYPE CHAR1,
***         LV_DATE       TYPE SY-DATUM,
***         L_VAR_PWD_LEN  TYPE I,
***         L_VAR_HANDLE   TYPE I,
***         G_VAR_RESPONSE TYPE CHAR1,
***        L_TAB_DATA      TYPE TABLE OF TEXT,
***        L_VAR_PATH(100) TYPE C,
***        LV_SHIP_DATE(10)     TYPE C.
***Constant*************************************************************
***  CONSTANTS : L_CON_KEY       TYPE I VALUE 26101957. "Hardcoded Handler Key,This is always '26101957'
************************************************************************
***  IF SY-BATCH = ABAP_FALSE.
***    CALL FUNCTION 'POPUP_TO_CONFIRM'
***      EXPORTING
***        TITLEBAR              = 'Confirmation'
***        TEXT_QUESTION         = 'Do you want to create the file on FTP?'
***        TEXT_BUTTON_1         = 'Yes'
***        TEXT_BUTTON_2         = 'No'
***        DEFAULT_BUTTON        = '1'
***        DISPLAY_CANCEL_BUTTON = ''
***      IMPORTING
***        ANSWER                = LV_ANSWER.
***    IF LV_ANSWER = 2.
***      MESSAGE e004(zgsp_utility).
***      MESSAGE 'Not Allowed to download' TYPE 'E'.
***    ENDIF.
***  ENDIF.
***  IF GT_FINAL_OUTWARD IS NOT INITIAL.
***    SORT GT_FINAL_OUTWARD BY LOCATION_GSTIN DOCUMENT_NO.
***    SELECT SINGLE MANDT
***           FILE_TYPE
***           FILE_TYPE_DISC
***           FTP_HOST
***           FTP_USER
***           FTP_PASSWARD
***           FTP_EXTENSION
***           RFC_DESTINATION
***           REQUEST_CAMMAND
***           REQ_COM_ARCH
***           RESPONSE_ALL_FILE_CMD
***           RESPONSE_COMMAND FROM ZDM_CUST_DATA INTO LS_CUST_DATA WHERE FILE_TYPE = '01'.
***    L_VAR_PWD_LEN = STRLEN( LS_CUST_DATA-FTP_PASSWARD ).
***    CALL FUNCTION 'HTTP_SCRAMBLE' "For Encrypting the Password
***      EXPORTING
***        SOURCE      = LS_CUST_DATA-FTP_PASSWARD
***        SOURCELEN   = L_VAR_PWD_LEN
***        KEY         = L_CON_KEY
***      IMPORTING
***        DESTINATION = LS_CUST_DATA-FTP_PASSWARD.
***
***    CALL FUNCTION 'FTP_CONNECT' "For connecting to the FTP Server's user directory
***      EXPORTING
***        USER            = LS_CUST_DATA-FTP_USER
***        PASSWORD        = LS_CUST_DATA-FTP_PASSWARD
***        HOST            = LS_CUST_DATA-FTP_HOST
***        RFC_DESTINATION = LS_CUST_DATA-RFC_DESTINATION
***      IMPORTING
***        HANDLE          = L_VAR_HANDLE
***      EXCEPTIONS
***        NOT_CONNECTED   = 1
***        OTHERS          = 2.
***    IF SY-SUBRC <> 0.
***      G_VAR_RESPONSE = 1.
***      RETURN.
***    ELSE.
***      CALL FUNCTION 'FTP_COMMAND'
***        EXPORTING
***          HANDLE        = L_VAR_HANDLE
***          COMMAND       = LS_CUST_DATA-REQUEST_CAMMAND
***        TABLES
***          DATA          = L_TAB_DATA
***        EXCEPTIONS
***          COMMAND_ERROR = 1
***          TCPIP_ERROR   = 2.
***      IF SY-SUBRC <> 0.
***        G_VAR_RESPONSE = 2.
***        CALL FUNCTION 'FTP_DISCONNECT' "For Disconnecting the connected FTP Session
***          EXPORTING
***            HANDLE = L_VAR_HANDLE
***          EXCEPTIONS
***            OTHERS = 1.
***
***        CALL FUNCTION 'RFC_CONNECTION_CLOSE'
***          EXPORTING
***            DESTINATION = LS_CUST_DATA-RFC_DESTINATION
***          EXCEPTIONS
***            OTHERS      = 1.
***        RETURN.
***      ENDIF.
***    ENDIF.
*********************************************************************
***    SORT GT_FINAL_OUTWARD BY LOCATION_GSTIN.
***    LOOP AT GT_FINAL_OUTWARD INTO LS_OUT_INVOICE.
***      AT FIRST.
***        LS_FINAL-LOCATION_GSTIN               = 'LocationGSTIN'.
***        LS_FINAL-LOCATION_LEGAL_NAME          = 'LocationLegalName'.
***        LS_FINAL-DOCUMENT_PURPOSE             = 'Document Purpose'.
***        LS_FINAL-SUPPLY_TYPE                  = 'Supply Type'.
***        LS_FINAL-SUB_SUPPLY_TYPE              = 'Sub Supply Type'.
***        LS_FINAL-SUB_SUPPLY_DESCRIPTION       = 'Sub Supply Type Description'.
***        LS_FINAL-DOCUMENT_TYPE                = 'Document Type'.
***        LS_FINAL-DOCUMENT_NO                  = 'Document Number'.
***        LS_FINAL-DOCUMENT_DATE                = 'Document Date'.
***        LS_FINAL-REFERENCE_DOC_TYPE           = 'Reference Document Type'.
***        LS_FINAL-REFERENCE_DOC_NO             = 'Reference Document Number'.
***        LS_FINAL-REFERENCE_DOC_DATE           = 'Reference Document Date'.
***        LS_FINAL-TRANSACTION_TYPE             = 'Transaction Type'.
***        LS_FINAL-BILL_FROM_GSTIN              = 'Bill From GSTIN'.
***        LS_FINAL-BILL_FROM_OTH_PARTY_NAME     = 'Bill From Other Party Name'.
***        LS_FINAL-BILL_FROM_ADDRESS1           = 'Bill From Address 1'.
***        LS_FINAL-BILL_FROM_ADDRESS2           = 'Bill From Address 2'.
***        LS_FINAL-BILL_FROM_CITY               = 'Bill From City'.
***        LS_FINAL-BILL_FROM_STATE              = 'Bill From State'.
***        LS_FINAL-BILL_FROM_POST_CODE          = 'Bill From Postal Code'.
***        LS_FINAL-DISPATCH_FROM_GSTIN          = 'Dispatch From GSTIN'.
***        LS_FINAL-DISPATCH_FROM_OTH_PARTY_NAME = 'Dispatch From Other Party Name'.
***        LS_FINAL-DISPATCH_FROM_ADDRESS1       = 'Dispatch From Address1'.
***        LS_FINAL-DISPATCH_FROM_ADDRESS2       = 'Dispatch From Address2'.
***        LS_FINAL-DISPATCH_FROM_CITY           = 'Dispatch From City'.
***        LS_FINAL-DISPATCH_FROM_STATE          = 'Dispatch From State'.
***        LS_FINAL-DISPATCH_FROM_POST_CODE      = 'Dispatch From Postal Code'.
***        LS_FINAL-CTIN                         = 'CTIN'.
***        LS_FINAL-EMAIL                        = 'E-Mail'.
***        LS_FINAL-MOBILE                       = 'Mobile'.
***        LS_FINAL-BILL_TO_GSTIN                = 'Bill To GSTIN'.
***        LS_FINAL-BILL_TO_NAME                 = 'Bill To Name'.
***        LS_FINAL-BILL_TO_ADDRESS1             = 'Bill To Address1'.
***        LS_FINAL-BILL_TO_ADDRESS2             = 'Bill To Address2'.
***        LS_FINAL-BILL_TO_CITY                 = 'Bill To City'.
***        LS_FINAL-BILL_TO_STATE                = 'Bill To State'.
***        LS_FINAL-BILL_TO_POST_CODE            = 'Bill To Postal Code'.
***        LS_FINAL-SHIP_TO_GSTIN                = 'Ship To GSTIN'.
***        LS_FINAL-SHIP_TO_NAME                 = 'Ship To Name'.
***        LS_FINAL-SHIP_TO_ADDRESS1             = 'Ship To Address1'.
***        LS_FINAL-SHIP_TO_ADDRESS2             = 'Ship To Address2'.
***        LS_FINAL-SHIP_TO_CITY                 = 'Ship To City'.
***        LS_FINAL-SHIP_TO_STATE                = 'Ship To State'.
***        LS_FINAL-SHIP_TO_POST_CODE            = 'Ship To Postal Code'.
***        LS_FINAL-POS                          = 'Place Of Supply'.
***        LS_FINAL-DOCUMENT_VALUE               = 'Document Value'.
***        LS_FINAL-OUTSTANDING_AMOUNT           = 'Outstanding Amount'.
***        LS_FINAL-REFERENCE_NO                 = 'Reference No'.
***        LS_FINAL-DIFF_PERCENTAGE              = 'Differential Percentage'.
***        LS_FINAL-REV_CHARGES                  = 'Reverse Charges'.
***        LS_FINAL-GSTIN_OF_ECOMM               = 'GSTIN of eCommerce'.
***        LS_FINAL-SHIP_BILL_NO                 = 'Shipping Or Export Bill Number'.
***        LS_FINAL-SHIP_BILL_DATE               = 'Shipping Or Export Bill Date'.
***        LS_FINAL-PORT_CODE                    = 'Shipping Port Code'.
***        LS_FINAL-TRANS_ID                     = 'Transporter Id'.
***        LS_FINAL-TRANS_NAME                   = 'Transporter Name'.
***        LS_FINAL-TRANS_DATE                   = 'Transporter Date'.
***        LS_FINAL-TRANS_TIME                   = 'Transporter Time'.
***        LS_FINAL-DISTANCE                     = 'Distance'.
***        LS_FINAL-TRANS_MODE                   = 'Transport Mode'.
***        LS_FINAL-TRANS_DOC_NO                 = 'Transport Document Number'.
***        LS_FINAL-TRANS_DOC_DATE               = 'Transport Document Date'.
***        LS_FINAL-VEHICAL_NO                   = 'Vehical No'.
***        LS_FINAL-VEHICAL_TYPE                 = 'Vehical Type'.
***        LS_FINAL-ORIG_GSTIN                   = 'Original GSTIN'.
***        LS_FINAL-ORIG_DOC_NO                  = 'Original Document Number'.
***        LS_FINAL-ORIG_DOC_DATE                = 'Original Document Date'.
***        LS_FINAL-PRODUCT_NAME                 = 'Product Name'.
***        LS_FINAL-DESCRIPTION                  = 'Description'.
***        LS_FINAL-HSN                          = 'HSN'.
***        LS_FINAL-QUANTITY                     = 'Quantity'.
***        LS_FINAL-UOM                          = 'Unit Of Measurement'.
***        LS_FINAL-TAX_VALUE                    = 'Taxable Value'.
***        LS_FINAL-DISCOUNT_AMOUNT              = 'Discount Amount'.
***        LS_FINAL-IGST_RATE                    = 'Interated Tax Rate'.
***        LS_FINAL-IGST_AMOUNT                  = 'Interated Tax Amount'.
***        LS_FINAL-CGST_RATE                    = 'Central Tax Rate'.
***        LS_FINAL-CGST_AMOUNT                  = 'Central Tax Amount'.
***        LS_FINAL-SGST_RATE                    = 'Ut/State Tax Rate'.
***        LS_FINAL-SGST_AMOUNT                  = 'Ut/State Tax Amount'.
***        LS_FINAL-CESS_RATE                    = 'Cess Rate'.
***        LS_FINAL-CESS_AMT                     = 'Cess Amount'.
***        LS_FINAL-CESS_NON_ADVAL_RATE          = 'Cess Non Advalorem Rate'.
***        LS_FINAL-CESS_NON_ADVAL_AMT           = 'Cess Non Advalorem Amount'.
***        LS_FINAL-OTHER_CHARGES                = 'Other Charges'.
***        LS_FINAL-ELIG_ITC                     = 'Eligibility of ITC'.
***        LS_FINAL-INTR_ITC_AMT                 = 'Integrated ITC Tax Amount'.
***        LS_FINAL-CENT_ITC_AMT                 = 'Central ITC Tax Amount'.
***        LS_FINAL-UT_ITC_AMT                   = 'State/UT ITC Tax Amount'.
***        LS_FINAL-CESS_ITC_AMT                 = 'Cess ITC Amount'.
***        LS_FINAL-SERIES_CODE                  = 'Series Code'.
***        LS_FINAL-CUSTOM1                      = 'Custom1'.
***        LS_FINAL-CUSTOM2                      = 'Custom2'.
***        LS_FINAL-CUSTOM3                      = 'Custom3'.
***        LS_FINAL-CUSTOM4                      = 'Custom4'.
***        LS_FINAL-CUSTOM5                      = 'Custom5'.
***        LS_FINAL-CUSTOM6                      = 'Custom6'.
***        LS_FINAL-CUSTOM7                      = 'Custom7'.
***        LS_FINAL-CUSTOM8                      = 'Custom8'.
***        LS_FINAL-CUSTOM9                      = 'Custom9'.
***        LS_FINAL-CUSTOM10                     = 'Custom10'.
***        LS_FINAL-WERKS                        = 'Plant'.
***        LS_FINAL-BUKRS                        = 'Company Code'.
***        LS_FINAL-DOC_STATUS                   = 'Document Status'.
***        APPEND LS_FINAL TO LT_FINAL.
***        CLEAR: LS_FINAL.
***      ENDAT.
***      MOVE-CORRESPONDING LS_OUT_INVOICE TO LS_FINAL.
***
***      IF LS_OUT_INVOICE-CGST_RATE IS INITIAL.
***        CLEAR : LS_FINAL-CGST_RATE.
***      ENDIF.
***      IF LS_OUT_INVOICE-CGST_AMOUNT IS INITIAL.
***        CLEAR : LS_FINAL-CGST_AMOUNT.
***      ENDIF.
***      IF LS_OUT_INVOICE-SGST_RATE IS INITIAL.
***        CLEAR : LS_FINAL-SGST_RATE.
***      ENDIF.
***      IF LS_OUT_INVOICE-SGST_AMOUNT IS INITIAL.
***        CLEAR : LS_FINAL-SGST_AMOUNT.
***      ENDIF.
***      IF LS_OUT_INVOICE-IGST_RATE IS INITIAL.
***        CLEAR : LS_FINAL-IGST_RATE.
***      ENDIF.
***      IF LS_OUT_INVOICE-IGST_AMOUNT IS INITIAL.
***        CLEAR : LS_FINAL-IGST_AMOUNT.
***      ENDIF.
***
***      IF LS_OUT_INVOICE-POS = LS_OUT_INVOICE-BILL_TO_STATE.
***        IF LS_OUT_INVOICE-CGST_RATE IS INITIAL AND LS_OUT_INVOICE-SGST_RATE IS INITIAL AND LS_OUT_INVOICE-IGST_RATE IS INITIAL.
***          LS_FINAL-CGST_RATE = 0.
***          LS_FINAL-CGST_AMOUNT = 0.
***          LS_FINAL-SGST_RATE = 0.
***          LS_FINAL-SGST_AMOUNT = 0.
***        ENDIF.
***      ENDIF.
***      IF LS_OUT_INVOICE-POS <> LS_OUT_INVOICE-BILL_TO_STATE.
***        IF LS_OUT_INVOICE-SGST_RATE IS INITIAL AND LS_OUT_INVOICE-CGST_RATE IS NOT INITIAL AND LS_OUT_INVOICE-IGST_RATE IS INITIAL.
***          LS_FINAL-IGST_RATE = 0.
***          LS_FINAL-IGST_AMOUNT = 0.
***        ENDIF.
***      ENDIF.
***      IF LS_OUT_INVOICE-QUANTITY IS INITIAL.
***        LS_FINAL-QUANTITY = 1.
***      ENDIF.
***      IF LS_OUT_INVOICE-POS = 99 AND LS_OUT_INVOICE-SUB_SUPPLY_TYPE = 'EXPWOP' .
***        IF LS_OUT_INVOICE-IGST_RATE IS NOT INITIAL.
***        ELSE.
***          LS_FINAL-IGST_RATE = 0.
***        ENDIF.
***        LS_FINAL-IGST_AMOUNT = 0.
***        CLEAR: LS_FINAL-CGST_RATE,LS_FINAL-CGST_AMOUNT,LS_FINAL-SGST_AMOUNT,LS_FINAL-SGST_RATE.
***      ELSE.
***        IF LS_OUT_INVOICE-CGST_RATE IS INITIAL AND LS_OUT_INVOICE-SGST_RATE IS INITIAL AND LS_OUT_INVOICE-IGST_RATE IS INITIAL.
***          IF LS_OUT_INVOICE-BILL_FROM_STATE = LS_OUT_INVOICE-POS.
***            LS_FINAL-IGST_RATE = 0.
***            LS_FINAL-IGST_AMOUNT = 0.
***            CLEAR: LS_FINAL-CGST_RATE,LS_FINAL-CGST_AMOUNT,LS_FINAL-SGST_RATE,LS_FINAL-SGST_AMOUNT.
***          ELSE.
***            LS_FINAL-IGST_RATE = 0.
***            CLEAR: LS_FINAL-CGST_RATE,LS_FINAL-CGST_AMOUNT,LS_FINAL-SGST_AMOUNT,LS_FINAL-SGST_RATE.
***          ENDIF.
***        ENDIF.
***      ENDIF.
***
***      IF LS_OUT_INVOICE-DIFF_PERCENTAGE IS INITIAL.
***        CLEAR:LS_FINAL-DIFF_PERCENTAGE.
***      ENDIF.
***      IF LS_OUT_INVOICE-CESS_RATE IS INITIAL.
***        CLEAR:LS_FINAL-CESS_RATE.
***      ENDIF.
***      CLEAR: LS_FINAL-CENT_ITC_AMT,LS_FINAL-INTR_ITC_AMT,LS_FINAL-UT_ITC_AMT,LS_FINAL-ELIG_ITC,LS_FINAL-ORIG_DOC_DATE,LS_FINAL-CESS_ITC_AMT,
***             LS_FINAL-ORIG_DOC_NO,LS_FINAL-ORIG_DOC_DATE.
***      IF LS_OUT_INVOICE-CESS_AMT IS INITIAL.
***        CLEAR:LS_FINAL-CESS_AMT.
***      ENDIF.
***      IF LS_OUT_INVOICE-CESS_NON_ADVAL_RATE IS INITIAL.
***        CLEAR:LS_FINAL-CESS_NON_ADVAL_RATE.
***      ENDIF.
***      IF LS_OUT_INVOICE-CESS_NON_ADVAL_AMT IS INITIAL.
***        CLEAR:LS_FINAL-CESS_NON_ADVAL_AMT.
***      ENDIF.
***      IF LS_OUT_INVOICE-ELIG_ITC IS INITIAL.
***        CLEAR:LS_FINAL-INTR_ITC_AMT.
***        CLEAR:LS_FINAL-CENT_ITC_AMT.
***        CLEAR:LS_FINAL-UT_ITC_AMT.
***        CLEAR:LS_FINAL-UT_ITC_AMT.
***        CLEAR:LS_FINAL-CESS_ITC_AMT.
***      ENDIF.
***      IF LS_OUT_INVOICE-REFERENCE_DOC_DATE IS INITIAL.
***        CLEAR:LS_FINAL-REFERENCE_DOC_DATE.
***      ENDIF.
***      IF LS_OUT_INVOICE-TRANS_DATE IS INITIAL.
***        CLEAR:LS_FINAL-TRANS_DATE.
***      ENDIF.
***      IF LS_OUT_INVOICE-TRANS_DOC_DATE IS INITIAL.
***        CLEAR:LS_FINAL-TRANS_DOC_DATE.
***      ENDIF.
***      IF LS_OUT_INVOICE-OTHER_CHARGES IS INITIAL.
***        CLEAR:LS_FINAL-OTHER_CHARGES.
***      ENDIF.
***      IF LS_OUT_INVOICE-OUTSTANDING_AMOUNT IS INITIAL.
***        CLEAR:LS_FINAL-OUTSTANDING_AMOUNT.
***      ENDIF.
***      IF LS_OUT_INVOICE-DISCOUNT_AMOUNT IS INITIAL.
***        CLEAR:LS_FINAL-DISCOUNT_AMOUNT.
***      ENDIF.
***      CALL FUNCTION 'CLOI_PUT_SIGN_IN_FRONT'
***        CHANGING
***          VALUE = LS_FINAL-TAX_VALUE.
***      CALL FUNCTION 'CLOI_PUT_SIGN_IN_FRONT'
***        CHANGING
***          VALUE = LS_FINAL-IGST_AMOUNT.
***      CALL FUNCTION 'CLOI_PUT_SIGN_IN_FRONT'
***        CHANGING
***          VALUE = LS_FINAL-CGST_AMOUNT.
***      CALL FUNCTION 'CLOI_PUT_SIGN_IN_FRONT'
***        CHANGING
***          VALUE = LS_FINAL-SGST_AMOUNT.
***      CALL FUNCTION 'CLOI_PUT_SIGN_IN_FRONT'
***        CHANGING
***          VALUE = LS_FINAL-CESS_AMT.
***      CALL FUNCTION 'CLOI_PUT_SIGN_IN_FRONT'
***        CHANGING
***          VALUE = LS_FINAL-DOCUMENT_VALUE.
***
***      IF LS_OUT_INVOICE-DOCUMENT_DATE IS NOT INITIAL.
***        CONCATENATE LS_FINAL-DOCUMENT_DATE+6(2) '.' LS_FINAL-DOCUMENT_DATE+4(2) '.' LS_FINAL-DOCUMENT_DATE+0(4)
***                    INTO LS_FINAL-DOCUMENT_DATE.
***      ELSE.
***        CLEAR:LS_FINAL-DOCUMENT_DATE.
***      ENDIF.
***
***      IF LS_OUT_INVOICE-SHIP_BILL_DATE IS NOT INITIAL.
***        LV_SHIP_DATE = LS_FINAL_OUTWARD_EY-SHIPPINGBILLDATE.
***        CONCATENATE LV_SHIP_DATE+8(2) '.' LV_SHIP_DATE+5(2) '.' LV_SHIP_DATE+0(4) INTO LS_FINAL_OUT-SHIP_BILL_DATE.
***      ELSE.
***        CLEAR:LS_FINAL-SHIP_BILL_DATE.
***
***      ENDIF.
***
***      CONDENSE:LS_OUT_INVOICE-ORIG_DOC_DATE.
***      IF LS_OUT_INVOICE-ORIG_DOC_DATE IS NOT INITIAL.
***        CONCATENATE LS_FINAL-ORIG_DOC_DATE+6(2) '.' LS_FINAL-ORIG_DOC_DATE+4(2) '.' LS_FINAL-ORIG_DOC_DATE+0(4)
***                    INTO LS_FINAL-ORIG_DOC_DATE.
***      ELSE.
***        CLEAR : LS_FINAL-ORIG_DOC_DATE.
***      ENDIF.
***
***      IF ls_final-orig_doc_date = 00000000 and ls_final-ship_bill_date = 00000000.
***        clear: ls_final-orig_doc_date,ls_final-ship_bill_date.
***      ENDIF.
***
*******SOA RK, on 27.11.2019
***
***      CONDENSE:LS_OUT_INVOICE-ORIG_DOC_DATE.
***      IF LS_OUT_INVOICE-ORIG_DOC_DATE IS NOT INITIAL.
***        CONCATENATE LS_OUT_INVOICE-ORIG_DOC_DATE+6(2) '-' LS_OUT_INVOICE-ORIG_DOC_DATE+4(2) '-' LS_OUT_INVOICE-ORIG_DOC_DATE+0(4)
***                    INTO LS_FINAL-ORIG_DOC_DATE.
***      ELSE.
***        CLEAR : LS_FINAL-ORIG_DOC_DATE.
***      ENDIF.
***
***      IF LS_OUT_INVOICE-SHIP_BILL_DATE IS NOT INITIAL.
***        LV_SHIP_DATE = LS_OUT_INVOICE-SHIP_BILL_DATE.
***        CONCATENATE LV_SHIP_DATE+8(2) '-' LV_SHIP_DATE+5(2) '-' LV_SHIP_DATE+0(4)
***        INTO LS_FINAL-SHIP_BILL_DATE.
***      ELSE.
***        CLEAR: LS_FINAL-SHIP_BILL_DATE.
***      ENDIF.
***
***      IF LS_OUT_INVOICE-DOCUMENT_DATE IS NOT INITIAL.
***        CONCATENATE LS_OUT_INVOICE-DOCUMENT_DATE+6(2) '-' LS_OUT_INVOICE-DOCUMENT_DATE+4(2) '-' LS_OUT_INVOICE-DOCUMENT_DATE+0(4)
***                    INTO LS_FINAL-DOCUMENT_DATE.
***      ELSE.
***        CLEAR:LS_FINAL-DOCUMENT_DATE.
***      ENDIF.
***
***      IF LS_OUT_INVOICE-TRANS_DOC_DATE IS NOT INITIAL.
***        CONCATENATE LS_OUT_INVOICE-TRANS_DOC_DATE+6(2) '-' LS_OUT_INVOICE-TRANS_DOC_DATE+4(2) '-' LS_OUT_INVOICE-TRANS_DOC_DATE+0(4)
***          INTO LS_FINAL-TRANS_DOC_DATE.
***      ELSE.
***        CLEAR : LS_FINAL-TRANS_DOC_DATE.
***      ENDIF.
***
***      IF LS_OUT_INVOICE-TRANS_DATE IS NOT INITIAL.
***        CONCATENATE LS_OUT_INVOICE-TRANS_DATE+6(2) '-' LS_OUT_INVOICE-TRANS_DATE+4(2) '-' LS_OUT_INVOICE-TRANS_DATE+0(4)
***          INTO LS_FINAL-TRANS_DATE.
***      ELSE.
***        CLEAR : LS_FINAL-TRANS_DATE.
***      ENDIF.
***
***      IF LS_OUT_INVOICE-REFERENCE_DOC_DATE IS NOT INITIAL.
***        CONCATENATE LS_OUT_INVOICE-REFERENCE_DOC_DATE+6(2) '-' LS_OUT_INVOICE-REFERENCE_DOC_DATE+4(2) '-' LS_OUT_INVOICE-REFERENCE_DOC_DATE+0(4)
***          INTO LS_FINAL-REFERENCE_DOC_DATE.
***      ELSE.
***        CLEAR : LS_FINAL-REFERENCE_DOC_DATE.
***      ENDIF.
***
***      IF LS_OUT_INVOICE-CUSTOM7 IS NOT INITIAL.
***        CONCATENATE LS_FINAL-CUSTOM7+6(2) '/' LS_FINAL-CUSTOM7+4(2) '/' LS_FINAL-CUSTOM7+0(4)
***          INTO LS_FINAL-CUSTOM7.
***      ELSE.
***        CLEAR : LS_FINAL-CUSTOM7.
***      ENDIF.
***
***      IF LS_OUT_INVOICE-CUSTOM3 IS NOT INITIAL.
***        CONCATENATE LS_OUT_INVOICE-CUSTOM3+0(2) '-' LS_OUT_INVOICE-CUSTOM3+3(2) '-' LS_OUT_INVOICE-CUSTOM3+6(4)
***          INTO LS_FINAL-CUSTOM3.
***      ELSE.
***        CLEAR : LS_FINAL-CUSTOM3.
***      ENDIF.
***
***      IF LS_FINAL-CUSTOM7 = '- -' OR LS_FINAL-CUSTOM7 = '--'.
***        CLEAR:LS_FINAL-CUSTOM7.
***      ENDIF.
***      IF LS_FINAL-ORIG_DOC_DATE = '- -' OR LS_FINAL-ORIG_DOC_DATE = '--'.
***        CLEAR:LS_FINAL-ORIG_DOC_DATE.
***      ENDIF.
***      IF LS_FINAL-SHIP_BILL_DATE = '- -' OR LS_FINAL-SHIP_BILL_DATE = '--'.
***        CLEAR:LS_FINAL-SHIP_BILL_DATE.
***      ENDIF.
***      IF LS_FINAL-REFERENCE_DOC_DATE = '- -' OR LS_FINAL-REFERENCE_DOC_DATE = '--'.
***        CLEAR: LS_FINAL-REFERENCE_DOC_DATE.
***      ENDIF.
***      IF LS_FINAL-TRANS_DATE = '- -' OR LS_FINAL-TRANS_DATE = '--'.
***        CLEAR: LS_FINAL-TRANS_DATE.
***      ENDIF.
***      IF LS_FINAL-TRANS_DOC_DATE = '- -' OR LS_FINAL-TRANS_DOC_DATE = '--'.
***        CLEAR: LS_FINAL-TRANS_DOC_DATE.
***      ENDIF.
***      IF LS_FINAL-DOCUMENT_DATE = '- -' OR LS_FINAL-DOCUMENT_DATE = '--'.
***        CLEAR: LS_FINAL-DOCUMENT_DATE.
***      ENDIF.
*******EOA RK, on 27.11.2019
***
***
***      APPEND LS_FINAL TO LT_FINAL.
***      CLEAR: LS_FINAL.
***
***      AT LAST.
***        IF LT_FINAL[] IS NOT INITIAL.
*** Data convert in CSV
***          CALL FUNCTION 'SAP_CONVERT_TO_TEX_FORMAT'
***            EXPORTING
***              I_FIELD_SEPERATOR    = ','
***            TABLES
***              I_TAB_SAP_DATA       = LT_FINAL
***            CHANGING
***              I_TAB_CONVERTED_DATA = LT_OUTWARD_INVOICE
***            EXCEPTIONS
***              CONVERSION_FAILED    = 1
***              OTHERS               = 2.
***          IF SY-SUBRC = 0.
***            GET TIME STAMP FIELD LV_TSL.
***            LV_TIME_STAMP = LV_TSL.
***            lv_path = 'C:\Cygnet\'.
***            DATA: TEST TYPE STRING."Document_5536_08_2019_090919170404.xlsx "Time Stamp
***            CONCATENATE 'Document_5536_' s_DATE-LOW+4(2)'_' P_DATE-LOW+0(4) '_' P_DATE-LOW+6(2)  P_DATE-LOW+4(2)  P_DATE-LOW+0(4) SY-UZEIT '.' LV_TIME_STAMP+14(3) '.' LS_CUST_DATA-FTP_EXTENSION INTO L_VAR_PATH.
***            CONCATENATE 'Document_5536_' P_DATE-LOW '.' LV_TIME_STAMP+14(3) '.' LS_CUST_DATA-FTP_EXTENSION INTO L_VAR_PATH.
***            CONDENSE L_VAR_PATH.
***            CALL FUNCTION 'FTP_R3_TO_SERVER' "For Creating a file from SAP R3 to FTP Server
***           EXPORTING
***             HANDLE         = L_VAR_HANDLE
***             FNAME          = L_VAR_PATH
***             CHARACTER_MODE = ABAP_TRUE
***           TABLES
***             TEXT           = LT_OUTWARD_INVOICE "Final Internal table to be written to the text file in the FTP Server's Directory
***           EXCEPTIONS
***             TCPIP_ERROR    = 1
***             COMMAND_ERROR  = 2
***             DATA_ERROR     = 3
***             OTHERS         = 4.
***
***            IF SY-SUBRC <> 0."When FTP connection Fails
***              G_VAR_RESPONSE = 5.
***              EXIT.
***            ELSE.
***              G_VAR_RESPONSE = 0.
***              FREE: LT_FINAL[], GT_FINAL_OUTWARD[].
***            ENDIF.
***          ENDIF.
***        ELSE.
***          LV_VAR_RESPONSE = 3.
***        ENDIF.
***      ENDAT.
***      CLEAR : LS_OUT_INVOICE.
***    ENDLOOP.
***    CALL FUNCTION 'FTP_DISCONNECT' "For Disconnecting the connected FTP Session
***      EXPORTING
***        HANDLE = L_VAR_HANDLE
***      EXCEPTIONS
***        OTHERS = 1.
***
***    CALL FUNCTION 'RFC_CONNECTION_CLOSE'
***      EXPORTING
***        DESTINATION = LS_CUST_DATA-RFC_DESTINATION
***      EXCEPTIONS
***        OTHERS      = 1.
***  ELSE.
***    LV_VAR_RESPONSE = 4.
***  ENDIF.

*ENDFORM.                    " FILE_GENERATE
*&---------------------------------------------------------------------*
*&      Form  GET_DATA_FROM_STAGE
*** Generate Data for GSTR1 Filing from stagging Table
*** Table name :- YGSTOUTWARD01
*** Created By :- Aditya Patel.
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  UPDATE_STAG_TABLE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  TOP_OF_PAGE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM TOP_OF_PAGE .

*TITLE
  CLEAR: LT_HEADER,LS_HEADER.

  LS_HEADER-TYP = 'H'.
  LS_HEADER-INFO = 'GSTR1 Generator'.
  APPEND LS_HEADER TO LT_HEADER.
  CLEAR LS_HEADER.

*DATE
  LS_HEADER-TYP = 'S'.
  CONCATENATE 'Date: ' ' '  S_DATE-LOW+6(2) '.' S_DATE-LOW+4(2) '.' S_DATE-LOW(4) ' ' 'To' ' '  S_DATE-HIGH+6(2) '.' S_DATE-HIGH+4(2) '.' S_DATE-HIGH(4)
              INTO LS_HEADER-INFO.
  APPEND LS_HEADER TO LT_HEADER.
  CLEAR: LS_HEADER.

*TOTAL NO. OF RECORDS SELECTED
  DESCRIBE TABLE GT_FINAL_OUTWARD LINES L_VAR_CNT.
  LD_LINESC = L_VAR_CNT.
  LS_HEADER-TYP = 'S'.
  CONCATENATE 'Number of records found : ' LD_LINESC  INTO LS_HEADER-INFO.

*ls_HEADER-INFO = T_LINE.
  APPEND LS_HEADER TO LT_HEADER.
  CLEAR: LS_HEADER, T_LINE.


  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      IT_LIST_COMMENTARY = LT_HEADER.
ENDFORM.                    " TOP_OF_PAGE
*&---------------------------------------------------------------------*
*&      Form  FILE_GENERATE_GL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form GET_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM GET_DATA .
  SELECT  * FROM ZDMDOCTYPE
   INTO TABLE L_TAB_DOCTYPE
   WHERE  SUPPLYTYPE = 'O'.

  SELECT * FROM ZEI_BUS_ID
    INTO TABLE L_TAB_BUS_ID.

  SELECT * FROM ZEI_BUS_UNIT_ID
    INTO TABLE L_TAB_BUS_UNIT_ID.

  SELECT * FROM ZEI_STATE_CODE
     INTO TABLE L_TAB_ZGST_STATE_CODE.

  SELECT * FROM ZEI_UOM_MAPING
    INTO TABLE L_TAB_ZEI_UOM_MAPING.

  IF L_TAB_DOCTYPE IS NOT INITIAL.
    SELECT VBRK~VBELN
          VBRK~GJAHR
         VBRP~POSNR
         VBRK~KNUMV
         VBRK~FKART
         VBRK~FKTYP
         VBRK~VBTYP
         VBRK~WAERK
         VBRK~VKORG
         VBRK~FKDAT
         VBRK~KURRF
         VBRK~BUPLA
         VBRK~BUKRS
         VBRK~REGIO
         VBRK~NETWR
         VBRK~KUNRG
         VBRK~KUNAG
         VBRK~SFAKN
         VBRK~XBLNR
         VBRK~ZUONR
         VBRK~MWSBK
         VBRK~FKSTO
         VBRK~RFBSK
         VBRP~AUBEL
         VBRP~FKIMG
         VBRP~NETWR AS I_NETWR
         VBRP~MATNR
         VBRP~ARKTX
         VBRP~WERKS
         VBRP~VRKME
         INTO TABLE L_TAB_VBRK
                    FROM VBRK AS VBRK  INNER JOIN VBRP AS VBRP
                    ON VBRK~VBELN = VBRP~VBELN
                    FOR ALL ENTRIES IN L_TAB_DOCTYPE
                    WHERE VBRK~FKART = L_TAB_DOCTYPE-FKART AND
                        VBRK~FKDAT IN S_DATE
                    AND VBRK~BUKRS IN S_BUKRS
                    AND VBRK~BUPLA IN S_BUPLA
                    AND VBRK~FKART IN S_TYPE
                    AND VBRK~VBELN IN S_VBELN
*                      AND vbrk~vbeln = s_vebln
                    AND RFBSK = 'C'
                    AND VBRP~WERKS IN  S_PLANT
                    AND VBRK~FKSTO <> 'X'.
*                    AND   vbrk~vbtyp IN ('M','S','P','5','O','N','6')
*                    AND   vbrk~fkart NOT IN ('ZSN').
    IF L_TAB_VBRK IS NOT INITIAL.
      SELECT BUKRS  BELNR GJAHR XBLNR INTO TABLE L_TAB_BKPF1
        FROM BKPF FOR ALL ENTRIES IN L_TAB_VBRK
        WHERE XBLNR = L_TAB_VBRK-XBLNR ."AND gjahr = s_gjahr.
    ENDIF.
    IF L_TAB_BKPF1 IS NOT INITIAL.
      SELECT BUKRS BELNR GJAHR FROM BSET INTO TABLE L_TAB_BSET1
        FOR ALL ENTRIES IN L_TAB_BKPF1
        WHERE BUKRS = L_TAB_BKPF1-BUKRS AND BELNR = L_TAB_BKPF1-BELNR AND GJAHR = L_TAB_BKPF1-GJAHR.
    ENDIF.


    CLEAR : L_TAB_BKPF,L_TAB_BSEG,L_TAB_T001W,L_TAB_KNA1,L_TAB_BSET,L_TAB_J_1BBRANCH.
    SELECT BUKRS
           BELNR
           GJAHR
           AWKEY
           AWTYP
           XBLNR
           BUDAT
           BLART
           BLDAT
           WAERS
           KURSF
      FROM BKPF INTO TABLE L_TAB_BKPF_FI
                 FOR ALL ENTRIES IN L_TAB_DOCTYPE
                 WHERE
                       BLART = L_TAB_DOCTYPE-FKART+0(2) AND
                       BELNR IN S_VBELN AND
*                       belnr = s_vebln AND
**                       blart = 'DR' AND "IN ( 'DR', 'ZD', 'ZE' )  AND
                       BUDAT IN S_DATE AND
                       BUKRS IN S_BUKRS AND
                       BLART IN S_TYPE AND
                       XREVERSAL = ''.
  ENDIF.
  IF L_TAB_BKPF_FI IS NOT INITIAL.
    SELECT BUKRS
           BELNR
           VBELN
           GJAHR
           BUZEI
           TXGRP
           BUZID
           BUPLA
           HSN_SAC
           PLC_SUP
           GST_PART
           MATNR
           WERKS
           WRBTR
           KUNNR
           MENGE
           HKONT
           GSBER
           LIFNR
           SGTXT
             FROM BSEG INTO TABLE L_TAB_BSEG
                 FOR ALL ENTRIES IN L_TAB_BKPF_FI
                 WHERE BUKRS = L_TAB_BKPF_FI-BUKRS AND
                       BELNR = L_TAB_BKPF_FI-BELNR AND
                       GJAHR = L_TAB_BKPF_FI-GJAHR AND
                       BUZID = ' ' AND
                       TXGRP <> ' ' AND
                       BUPLA IN S_BUPLA AND
                       KOART NE 'K'.

    IF L_TAB_BSEG IS NOT INITIAL.
      SELECT MATNR
             WERKS
             STEUC FROM MARC APPENDING TABLE L_TAB_MARC
              FOR ALL ENTRIES IN L_TAB_BSEG
              WHERE MATNR = L_TAB_BSEG-MATNR
                AND WERKS = L_TAB_BSEG-WERKS.


      SELECT BUKRS
          BELNR
          VBELN
          GJAHR
          BUZEI
          TXGRP
          BUZID
          BUPLA
          HSN_SAC
          PLC_SUP
          GST_PART
          MATNR
          WERKS
          WRBTR
          KUNNR
          MENGE
          HKONT
          GSBER
          LIFNR
          SGTXT
            FROM BSEG INTO TABLE L_TAB_BSEG_TEMP
                FOR ALL ENTRIES IN L_TAB_BSEG
                WHERE BUKRS = L_TAB_BSEG-BUKRS AND
                      BELNR = L_TAB_BSEG-BELNR AND
                      GJAHR = L_TAB_BSEG-GJAHR AND
                      KUNNR NE ''.
    ENDIF.
    IF L_TAB_BSEG IS NOT INITIAL.
      SELECT WERKS
             NAME1
             PSTLZ
             ORT01
             J_1BBRANCH REGIO FROM T001W INTO TABLE L_TAB_T001W
                        FOR ALL ENTRIES IN L_TAB_BSEG
                        WHERE WERKS = L_TAB_BSEG-BUPLA. "  werks.

      IF L_TAB_T001W IS NOT INITIAL.
        SELECT BUKRS
               BRANCH
               NAME
               GSTIN
              FROM J_1BBRANCH
            INTO TABLE L_TAB_J_1BBRANCH
              FOR ALL ENTRIES IN L_TAB_T001W
              WHERE BRANCH = L_TAB_T001W-J_1BBRANCH.
      ENDIF.

      IF L_TAB_BSEG_TEMP IS NOT INITIAL.
        SELECT
          KUNNR
          LAND1
          NAME1
          REGIO
          STRAS
          ADRNR
          STCD3
          ORT01
          PSTLZ
          FROM KNA1 INTO TABLE L_TAB_KNA1_FI
           FOR ALL ENTRIES IN L_TAB_BSEG_TEMP
           WHERE KUNNR EQ L_TAB_BSEG_TEMP-KUNNR
           AND STCD3 NOT IN ('NA' ,'URD').
      ENDIF.

      SELECT BUKRS
             BELNR
             GJAHR
             BUZEI
             TXGRP
             SHKZG
             FWBAS
             FWSTE
             KSCHL
             KBETR BUPLA  FROM BSET
        INTO TABLE L_TAB_BSET
        FOR ALL ENTRIES IN L_TAB_BKPF_FI
        WHERE BUKRS = L_TAB_BKPF_FI-BUKRS AND
              BELNR = L_TAB_BKPF_FI-BELNR AND
              GJAHR = L_TAB_BKPF_FI-GJAHR.

*****        IF l_tab_kna1 IS NOT INITIAL.
*****          SELECT * FROM zsd_state_cd_map
*****            INTO TABLE  l_tab_state_cd_map_v
*****             FOR ALL ENTRIES IN l_tab_kna1
*****              WHERE region = l_tab_kna1-regio.
*****        ENDIF.
    ENDIF.
  ENDIF.

  IF L_TAB_VBRK IS NOT INITIAL.
    SELECT
        VBELV
        POSNV
        VBELN
        VBTYP_N
        VBTYP_V
    FROM VBFA
    INTO TABLE L_TAB_VBFA
    FOR ALL ENTRIES IN L_TAB_VBRK
    WHERE VBELN = L_TAB_VBRK-VBELN AND
          POSNV = L_TAB_VBRK-POSNR AND
          VBTYP_N = 'O' AND
          VBTYP_V = 'M'.

    IF L_TAB_VBFA IS NOT INITIAL.
      SELECT
        VBELN
        FKDAT
      FROM VBRK
        INTO TABLE L_TAB_VBRK_REFDATE
        FOR ALL ENTRIES IN L_TAB_VBFA
        WHERE VBELN = L_TAB_VBFA-VBELV.
    ENDIF.

    SELECT KUNNR LAND1 STCD3
      FROM KNA1
      INTO TABLE L_TAB_CUST_GSTIN
      FOR ALL ENTRIES IN L_TAB_VBRK
      WHERE KUNNR = L_TAB_VBRK-KUNAG.

    SELECT
      WERKS
      NAME1
      LIFNR
      PSTLZ
      ORT01
      REGIO
      FROM T001W
      INTO TABLE L_TAB_T001W1
      FOR ALL ENTRIES IN L_TAB_VBRK
      WHERE WERKS = L_TAB_VBRK-WERKS.
    IF L_TAB_T001W1 IS NOT INITIAL.
      SELECT LIFNR LAND1 STCD3
        INTO TABLE L_TAB_VEN_GSTIN
        FROM LFA1
        FOR ALL ENTRIES IN L_TAB_T001W1
        WHERE LIFNR = L_TAB_T001W1-LIFNR.
    ENDIF.

    SELECT KNUMV    "Tax Amoount Fetch
           KPOSN
           KSCHL
           KAWRT
           KBETR
           KWERT FROM PRCD_ELEMENTS "konv
                 INTO TABLE   L_TAB_KONV FOR ALL ENTRIES IN L_TAB_VBRK
*                 INTO TABLE l_tab_prcd_elements FOR ALL ENTRIES IN l_tab_vbrk
                 WHERE KNUMV = L_TAB_VBRK-KNUMV
                 AND   KPOSN = L_TAB_VBRK-POSNR
                 AND   KSCHL IN ('JOIG','JOCG','JOSG','JOUG','ZP01','ZMR1','ZSPC','ZPRS','ZART' ,'DIFF')
                 AND   KINAK = '' ."AND kbetr <> '0.00'.


    SELECT VBELN  "BILL TO PARTY
           POSNR
           PARVW
           KUNNR
           ADRNR
           LAND1 INTO TABLE L_TAB_VBPA
                 FROM VBPA FOR ALL ENTRIES IN L_TAB_VBRK
                 WHERE VBELN = L_TAB_VBRK-VBELN
                   AND PARVW = 'RE'.

    IF L_TAB_VBPA IS NOT INITIAL.
      SELECT KUNNR    "FETCH BILL TO PARTY DATA
             NAME1
             ORT01
             PSTLZ
             REGIO
             STCD3 LAND1 FROM KNA1
                   INTO TABLE L_TAB_KNA1
                   FOR ALL ENTRIES IN L_TAB_VBPA
                   WHERE KUNNR = L_TAB_VBPA-KUNNR.

    ENDIF.

    SELECT VBELN  "SHIP TO PARTY
           POSNR
           PARVW
           KUNNR
           ADRNR
           LAND1 INTO TABLE L_TAB_VBPA_TT
                 FROM VBPA FOR ALL ENTRIES IN L_TAB_VBRK
                 WHERE VBELN = L_TAB_VBRK-VBELN
                   AND PARVW = 'WE'.


    IF L_TAB_VBPA_TT IS NOT INITIAL.
      SELECT KUNNR    "FETCH BILL TO PARTY DATA
             NAME1
             ORT01
             PSTLZ
             REGIO
             STCD3 FROM KNA1
                   INTO TABLE L_TAB_KNA1_TT
                   FOR ALL ENTRIES IN L_TAB_VBPA_TT
                   WHERE KUNNR = L_TAB_VBPA_TT-KUNNR.
    ENDIF.

    SELECT
      BUKRS
      BRANCH
      NAME
      GSTIN
      FROM J_1BBRANCH
*      INTO TABLE l_tab_t001w
      INTO TABLE L_TAB_J_1BBRANCH
      FOR ALL ENTRIES IN L_TAB_VBRK
      WHERE BRANCH = L_TAB_VBRK-BUPLA
        AND BUKRS = L_TAB_VBRK-BUKRS
        AND GSTIN <> ''.

    IF L_TAB_VBRK IS NOT INITIAL.

      TYPES: BEGIN OF ITY_AWKEY,
               VBELN TYPE VBRK-VBELN,
               AWKEY TYPE BKPF-AWKEY,
             END OF ITY_AWKEY.


      LOOP AT L_TAB_VBRK INTO L_STR_VBRK.
        L_STR_AWKEY-VBELN = L_STR_VBRK-VBELN.
        L_STR_AWKEY-AWKEY = L_STR_VBRK-VBELN.
        COLLECT L_STR_AWKEY INTO L_TAB_AWKEY.
        CLEAR L_STR_AWKEY.
      ENDLOOP.

      IF L_TAB_AWKEY IS NOT INITIAL.
        SELECT BUKRS BELNR GJAHR AWKEY FROM BKPF
            INTO TABLE L_TAB_BKPF
            FOR ALL ENTRIES IN  L_TAB_AWKEY
            WHERE AWKEY = L_TAB_AWKEY-AWKEY.
      ENDIF.
    ENDIF.
  ENDIF.
*Direct fi
*  SELECT  * FROM zei_doc_type
*    INTO TABLE l_tab_doctype1  "where supp_type = 'S'.
*    WHERE gst = 'YES' AND supp_type = 'S'
*    and ewb = 'YES' AND sap_doc_type in ('DR' , 'DG' ).
  SELECT  * FROM ZDMDOCTYPE
    INTO TABLE L_TAB_DOCTYPE1  "where supp_type = 'S'.
    WHERE   SUPPLYTYPE = 'O'.
  "  AND blart = l_tab_doctype1-blart.
  SELECT BUKRS
    BELNR
    GJAHR
    AWKEY
    AWTYP
    XBLNR
    BUDAT
    BLART
    BLDAT
    WAERS
    KURSF  FROM BKPF INTO TABLE L_TAB_BKPF_DFI
    FOR ALL ENTRIES IN L_TAB_DOCTYPE1
    WHERE BLART =  L_TAB_DOCTYPE1-BLART
    AND BUKRS IN S_BUKRS
    AND BUDAT IN S_DATE..

  IF L_TAB_BKPF_DFI IS NOT INITIAL.
    SELECT BUKRS
          BELNR
          VBELN
          GJAHR
          BUZEI
          TXGRP
          BUZID
          BUPLA
          HSN_SAC
          PLC_SUP
          GST_PART
          MATNR
          WERKS
          WRBTR
          KUNNR
          MENGE
          HKONT
          GSBER
          LIFNR
          SGTXT
            FROM BSEG INTO TABLE L_TAB_BSEG_DFI
                FOR ALL ENTRIES IN L_TAB_BKPF_DFI
                WHERE BUKRS = L_TAB_BKPF_DFI-BUKRS AND
                      BELNR = L_TAB_BKPF_DFI-BELNR AND
                      GJAHR = L_TAB_BKPF_DFI-GJAHR AND
                      BUPLA IN S_BUPLA AND
                      KOART NE 'K'.

    SELECT BUKRS
BELNR
VBELN
GJAHR
BUZEI
TXGRP
BUZID
BUPLA
HSN_SAC
PLC_SUP
GST_PART
MATNR
WERKS
WRBTR
KUNNR
MENGE
HKONT
GSBER
LIFNR
SGTXT
FROM BSEG INTO TABLE L_TAB_BSEG_DFI_KUNNR
FOR ALL ENTRIES IN L_TAB_BKPF_DFI
WHERE BUKRS = L_TAB_BKPF_DFI-BUKRS AND
   BELNR = L_TAB_BKPF_DFI-BELNR AND
   GJAHR = L_TAB_BKPF_DFI-GJAHR AND
   KUNNR NE ' '  AND
   BUPLA IN S_BUPLA AND
   KOART NE 'K'.

*for kg type vendor credit memo
    SELECT BUKRS
BELNR
VBELN
GJAHR
BUZEI
TXGRP
BUZID
BUPLA
HSN_SAC
PLC_SUP
GST_PART
MATNR
WERKS
WRBTR
KUNNR
MENGE
HKONT
GSBER
LIFNR
SGTXT
FROM BSEG INTO TABLE L_TAB_BSEG_DFI_LIFNR
FOR ALL ENTRIES IN L_TAB_BKPF_DFI
WHERE BUKRS = L_TAB_BKPF_DFI-BUKRS AND
   BELNR = L_TAB_BKPF_DFI-BELNR AND
   GJAHR = L_TAB_BKPF_DFI-GJAHR AND
   LIFNR NE ' '  AND
   BUPLA IN S_BUPLA .

    SELECT BUKRS
BELNR
VBELN
GJAHR
BUZEI
TXGRP
BUZID
BUPLA
HSN_SAC
PLC_SUP
GST_PART
MATNR
WERKS
WRBTR
KUNNR
MENGE
HKONT
GSBER
LIFNR
SGTXT
FROM BSEG INTO TABLE L_TAB_BSEG_DFI_HSN
FOR ALL ENTRIES IN L_TAB_BKPF_DFI
WHERE BUKRS = L_TAB_BKPF_DFI-BUKRS AND
BELNR = L_TAB_BKPF_DFI-BELNR AND
GJAHR = L_TAB_BKPF_DFI-GJAHR AND
 BUPLA IN S_BUPLA  and      " KS
HSN_SAC NE ' ' .

    SELECT BUKRS
BELNR
VBELN
GJAHR
BUZEI
TXGRP
BUZID
BUPLA
HSN_SAC
PLC_SUP
GST_PART
MATNR
WERKS
WRBTR
KUNNR
MENGE
HKONT
GSBER
LIFNR
SGTXT
FROM BSEG INTO TABLE L_TAB_BSEG_DFI_SGTXT
FOR ALL ENTRIES IN L_TAB_BKPF_DFI
WHERE BUKRS = L_TAB_BKPF_DFI-BUKRS AND
BELNR = L_TAB_BKPF_DFI-BELNR AND
GJAHR = L_TAB_BKPF_DFI-GJAHR AND
BUPLA IN S_BUPLA  and      " KS
SGTXT NE  ' ' AND BSCHL = '01' AND KOART = 'D'.

    SELECT BUKRS
BELNR
VBELN
GJAHR
BUZEI
TXGRP
BUZID
BUPLA
HSN_SAC
PLC_SUP
GST_PART
MATNR
WERKS
WRBTR
KUNNR
MENGE
HKONT
GSBER
LIFNR
SGTXT
FROM BSEG INTO TABLE L_TAB_BSEG_DFI_KIDNO
FOR ALL ENTRIES IN L_TAB_BKPF_DFI
WHERE BUKRS = L_TAB_BKPF_DFI-BUKRS AND
BELNR = L_TAB_BKPF_DFI-BELNR AND
GJAHR = L_TAB_BKPF_DFI-GJAHR AND
      BUPLA IN S_BUPLA  and      " KS
KOART = 'S' AND KIDNO NE ' ' .

  ENDIF.

  DELETE L_TAB_BSEG_DFI_SGTXT WHERE SGTXT NS 'DN'.
  LOOP AT L_TAB_BSEG_DFI_SGTXT INTO L_STR_BSEG_DFI_SGTXT.
*        if l_str_bseg_dfi_sgtxt-sgtxt+0(4) = 'DN'.
    IF L_STR_BSEG_DFI_SGTXT-SGTXT+0(7) = 'DN: F22'.
      L_STR_TEMP-SGTXT  = L_STR_BSEG_DFI_SGTXT-SGTXT+4(12).
      L_STR_TEMP-GJAHR  = L_STR_BSEG_DFI_SGTXT-GJAHR.
      APPEND L_STR_TEMP TO L_TAB_TEMP.
    ENDIF.

*          ENDIF.


  ENDLOOP.

  IF L_TAB_TEMP IS NOT INITIAL.

    SELECT BUKRS
       BELNR
       GJAHR
       AWKEY
       AWTYP
       XBLNR
       BUDAT
       BLART
       BLDAT
       WAERS
       KURSF  FROM BKPF INTO TABLE L_TAB_BKPF_DFI_SGTXT
       FOR ALL ENTRIES IN L_TAB_TEMP
       WHERE XBLNR = L_TAB_TEMP-SGTXT AND
             GJAHR = L_TAB_TEMP-GJAHR.
  ENDIF.

  IF L_TAB_BSEG_DFI_KIDNO IS NOT INITIAL.
    SELECT BUKRS
BELNR
VBELN
GJAHR
BUZEI
TXGRP
BUZID
BUPLA
HSN_SAC
PLC_SUP
GST_PART
MATNR
WERKS
WRBTR
KUNNR
MENGE
HKONT
GSBER
LIFNR
SGTXT
FROM BSEG INTO TABLE L_TAB_BSEG_DFI_SGTXT1
FOR ALL ENTRIES IN L_TAB_BSEG_DFI_KIDNO
WHERE BUKRS = L_TAB_BSEG_DFI_KIDNO-BUKRS AND
BELNR = L_TAB_BSEG_DFI_KIDNO-KIDNO(10) AND
GJAHR = L_TAB_BSEG_DFI_KIDNO-GJAHR AND
SGTXT NE  ' '  AND BSCHL = '01' AND KOART = 'D'..

  ENDIF.


  DELETE L_TAB_BSEG_DFI_SGTXT1 WHERE SGTXT NS 'DN'.
  LOOP AT L_TAB_BSEG_DFI_SGTXT1 INTO L_STR_BSEG_DFI_SGTXT1.
*        if l_str_bseg_dfi_sgtxt-sgtxt+0(4) = 'DN'.
    IF L_STR_BSEG_DFI_SGTXT1-SGTXT+0(7) = 'DN: F22'.
      L_STR_TEMP1-SGTXT  = L_STR_BSEG_DFI_SGTXT1-SGTXT+4(12).
      L_STR_TEMP1-GJAHR = L_STR_BSEG_DFI_SGTXT1-GJAHR.
      APPEND L_STR_TEMP1 TO L_TAB_TEMP1.
    ENDIF.
  ENDLOOP.
  IF L_TAB_TEMP1 IS NOT INITIAL.

    SELECT BUKRS
       BELNR
       GJAHR
       AWKEY
       AWTYP
       XBLNR
       BUDAT
       BLART
       BLDAT
       WAERS
       KURSF  FROM BKPF INTO TABLE L_TAB_BKPF_DFI_KIDNO
       FOR ALL ENTRIES IN L_TAB_TEMP1
       WHERE XBLNR = L_TAB_TEMP1-SGTXT AND
             GJAHR = L_TAB_TEMP1-GJAHR.
  ENDIF.




  SELECT BUKRS
BELNR
GJAHR
BUZEI
TXGRP
SHKZG
FWBAS
FWSTE
KSCHL
KBETR BUPLA  FROM BSET
INTO TABLE L_TAB_BSET_DFI
FOR ALL ENTRIES IN L_TAB_BSEG_DFI
WHERE BUKRS = L_TAB_BSEG_DFI-BUKRS AND
BELNR = L_TAB_BSEG_DFI-BELNR AND
GJAHR = L_TAB_BSEG_DFI-GJAHR.



  SELECT
  KUNNR
  LAND1
  NAME1
  REGIO
  STRAS
  ADRNR
  STCD3
  ORT01
  PSTLZ
  FROM KNA1 INTO TABLE L_TAB_KNA1_DFI
   FOR ALL ENTRIES IN L_TAB_BSEG_DFI_KUNNR
   WHERE KUNNR EQ L_TAB_BSEG_DFI_KUNNR-KUNNR
   AND STCD3 NOT IN ('NA' ,'URD').
  IF L_TAB_BSEG_DFI_LIFNR IS NOT INITIAL.
    SELECT
    KUNNR
    LAND1
    NAME1
    REGIO
    ORT01
    FROM LFA1 INTO TABLE L_TAB_LFA1_DFI
         FOR ALL ENTRIES IN L_TAB_BSEG_DFI_LIFNR
         WHERE LIFNR EQ L_TAB_BSEG_DFI_LIFNR-LIFNR
         AND STCD3 NOT IN ('NA' ,'URD').
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM SET_DATA .
  SORT L_TAB_VBRK             BY VBELN POSNR KNUMV.
  SORT L_TAB_KONV             BY KNUMV KPOSN KSCHL.
*  SORT l_tab_prcd_elements             BY knumv kposn kschl.
*  SORT l_tab_bus_id           BY bukrs.
*  SORT l_tab_bus_unit_id      BY j_1bbranch.
  SORT L_TAB_KNA1             BY KUNNR.
  SORT L_TAB_VBPA             BY VBELN.
  SORT L_TAB_T001W            BY WERKS.
  SORT L_TAB_J_1BBRANCH       BY BUKRS BRANCH.
  SORT L_TAB_BSET_DFI         BY BUKRS BELNR GJAHR.
  SORT L_TAB_BKPF1            BY BUKRS BELNR GJAHR XBLNR.
*  SORT l_tab_zgst_state_code  BY regio.


  LOOP AT L_TAB_VBRK INTO L_STR_VBRK.
    CLEAR L_STR_FINAL.
*if l_str_vbrk-waerk ne 'INR'.
*  READ TABLE L_TAB_BKPF1 INTO L_STR_BKPF1 WITH KEY xblnr  = l_str_vbrk-xblnr .
*  IF SY-SUBRC = 0.
*  READ TABLE L_tAB_BSET INTO L_STR_BSET WITH KEY bukrs = L_sTR_BKPF1-bukrs belnr = L_sTR_BKPF1-belnr gjahr = L_sTR_BKPF1-gjahr BINARY SEARCH.
*  IF SY-SUBRC = 0 .
*    ELSE.
*      CONTINUE.
*      ENDIF.
*      ENDIF.
*      ENDIF.
    L_STR_FINAL-custom10 = L_STR_VBRK-BUPLA.
    READ TABLE L_TAB_AWKEY INTO L_STR_AWKEY WITH KEY VBELN = L_STR_VBRK-VBELN.
    IF SY-SUBRC = 0.
      READ TABLE L_TAB_BKPF INTO DATA(L_STR_BKPF) WITH KEY AWKEY = L_STR_AWKEY-AWKEY.
      IF SY-SUBRC = 0.
        L_STR_FINAL-CUSTOM4 = L_STR_BKPF-BELNR.
      ENDIF.
    ENDIF.

    READ TABLE L_TAB_CUST_GSTIN INTO L_STR_CUST_GSTIN WITH KEY KUNNR = L_STR_VBRK-KUNAG.
    IF SY-SUBRC = 0..
      CUST_GSTIN = L_STR_CUST_GSTIN-STCD3.
    ENDIF.

    READ TABLE L_TAB_T001W1 INTO L_STR_T001W1 WITH KEY WERKS = L_STR_VBRK-WERKS.
    IF SY-SUBRC = 0.
      READ TABLE L_TAB_VEN_GSTIN INTO L_STR_VEN_GSTIN WITH KEY LIFNR = L_STR_T001W1-LIFNR.
      IF SY-SUBRC = 0.
        VEN_GSTIN = L_STR_VEN_GSTIN-STCD3.
      ENDIF.
    ENDIF.

*    IF cust_gstin = ven_gstin.
*      CLEAR: l_str_final,l_str_cust_gstin,l_str_t001w,l_str_ven_gstin,l_str_vbrk.
*      CONTINUE.
*    ENDIF.

*    READ TABLE l_tab_bus_id INTO l_str_bus_id WITH KEY bukrs = l_str_vbrk-bukrs BINARY SEARCH.
*    IF sy-subrc NE 0.
*      CLEAR: l_str_final,l_str_cust_gstin,l_str_t001w,l_str_ven_gstin,l_str_vbrk, l_str_bus_id.
*      CONTINUE.
*    ENDIF.
*    READ TABLE l_tab_bus_unit_id INTO l_str_bus_unit_id WITH KEY BUS_PLACE = l_str_vbrk-bupla BINARY SEARCH.
*    IF sy-subrc NE 0.
*      CLEAR: l_str_final,l_str_cust_gstin,l_str_t001w,l_str_ven_gstin,l_str_vbrk, l_str_bus_id, l_str_bus_unit_id.
*      CONTINUE.
*    ENDIF.

    READ TABLE L_TAB_DOCTYPE INTO L_STR_DOCTYPE WITH KEY SUPPLYTYPE = 'O'
                                                         FKART = L_STR_VBRK-FKART.
    IF SY-SUBRC = 0.
      L_STR_FINAL-DOCUMENT_TYPE = L_STR_DOCTYPE-DOCTYPE.
    ENDIF.

    IF L_STR_FINAL-DOCUMENT_TYPE = 'CRN' OR L_STR_FINAL-DOCUMENT_TYPE = 'DRN'.
      READ TABLE L_TAB_VBFA INTO L_STR_VBFA WITH KEY VBELN = L_STR_VBRK-VBELN POSNV = L_STR_VBRK-POSNR.
      IF SY-SUBRC = 0.
        L_STR_FINAL-REFERENCE_DOC_NO = L_STR_VBFA-VBELV.
        IF L_STR_FINAL-REFERENCE_DOC_DATE IS NOT INITIAL.
          READ TABLE L_TAB_VBRK_REFDATE INTO L_STR_VBRK_REFDATE WITH KEY  VBELN = L_STR_FINAL-REFERENCE_DOC_NO.
          L_STR_FINAL-REFERENCE_DOC_DATE   = L_STR_VBRK_REFDATE-FKDAT.
        ENDIF.
      ENDIF.
    ENDIF.



    DATA: LV_VAR TYPE CHAR10.

    SELECT SINGLE STEUC INTO LV_VAR
      FROM MARC
      WHERE MATNR =  L_STR_VBRK-MATNR AND
            WERKS  = L_STR_VBRK-WERKS .    "l_str_final-hsn

    L_STR_FINAL-HSN = LV_VAR.
    L_STR_FINAL-PRODUCT_NAME = L_STR_VBRK-MATNR.
    L_STR_FINAL-DESCRIPTION = L_STR_VBRK-ARKTX.
    L_STR_FINAL-QUANTITY = L_STR_FINAL-QUANTITY + L_STR_VBRK-FKIMG.

    READ TABLE L_TAB_ZEI_UOM_MAPING INTO L_STR_ZEI_UOM_MAPING WITH KEY ZSAP_UOM = L_STR_VBRK-VRKME.
    IF SY-SUBRC = 0.
      L_STR_FINAL-UOM = L_STR_ZEI_UOM_MAPING-ZGSP_UOM.
    ENDIF.
*    expwop logic
    READ TABLE L_TAB_VBPA INTO L_STR_VBPA WITH KEY VBELN = L_STR_VBRK-VBELN BINARY SEARCH.
    IF SY-SUBRC = 0.
      READ TABLE L_TAB_KNA1 INTO L_STR_KNA1 WITH KEY KUNNR = L_STR_VBPA-KUNNR BINARY SEARCH.
    ENDIF.
    IF L_STR_VBRK-WAERK NE 'INR'.
      READ TABLE L_TAB_BKPF1 INTO L_STR_BKPF1 WITH KEY XBLNR  = L_STR_VBRK-XBLNR .
      IF SY-SUBRC = 0.
        READ TABLE L_TAB_BSET1 INTO L_STR_BSET1 WITH KEY BUKRS = L_STR_BKPF1-BUKRS BELNR = L_STR_BKPF1-BELNR GJAHR = L_STR_BKPF1-GJAHR BINARY SEARCH.
        IF SY-SUBRC = 0 .
        ELSE.
          READ TABLE L_TAB_KONV INTO L_STR_KONV WITH KEY KNUMV = L_STR_VBRK-KNUMV
                                                        KPOSN = L_STR_VBRK-POSNR KSCHL = 'JOIG' BINARY SEARCH.
          L_STR_FINAL-TAX_VALUE = L_STR_KONV-KAWRT.
*      CONTINUE.
        ENDIF.
      ENDIF.
*      ENDIF.
    ELSE.
      READ TABLE L_TAB_KONV TRANSPORTING NO FIELDS WITH KEY KNUMV = L_STR_VBRK-KNUMV
                                                                  KPOSN = L_STR_VBRK-POSNR BINARY SEARCH.
      IF SY-SUBRC = 0.
        READ TABLE L_TAB_KONV INTO L_STR_KONV WITH KEY KNUMV = L_STR_VBRK-KNUMV
                                                       KPOSN = L_STR_VBRK-POSNR KSCHL = 'JOIG' BINARY SEARCH.
        IF SY-SUBRC = 0.
          L_STR_FINAL-TAX_VALUE = L_STR_KONV-KAWRT.
          L_STR_FINAL-IGST_AMOUNT    = L_STR_KONV-KWERT.
          L_STR_FINAL-IGST_RATE            = L_STR_KONV-KBETR.
        ENDIF.
      ENDIF.
    ENDIF.
    READ TABLE L_TAB_KONV INTO L_STR_KONV WITH KEY KNUMV = L_STR_VBRK-KNUMV
                                                   KPOSN = L_STR_VBRK-POSNR KSCHL = 'JOCG' BINARY SEARCH.
    IF SY-SUBRC = 0.
      L_STR_FINAL-TAX_VALUE = L_STR_KONV-KAWRT.
      L_STR_FINAL-CGST_AMOUNT    = L_STR_KONV-KWERT.
      L_STR_FINAL-CGST_RATE            = ( L_STR_FINAL-CGST_RATE + L_STR_KONV-KBETR ).
    ENDIF.
    READ TABLE L_TAB_KONV INTO L_STR_KONV WITH KEY KNUMV = L_STR_VBRK-KNUMV
                                                   KPOSN = L_STR_VBRK-POSNR KSCHL = 'JOSG' BINARY SEARCH.
    IF SY-SUBRC = 0.
      L_STR_FINAL-TAX_VALUE = L_STR_KONV-KAWRT.
      L_STR_FINAL-SGST_AMOUNT      = L_STR_KONV-KWERT.
      L_STR_FINAL-SGST_RATE            = ( L_STR_FINAL-SGST_RATE + L_STR_KONV-KBETR ).
    ENDIF.
    READ TABLE L_TAB_KONV INTO L_STR_KONV WITH KEY KNUMV = L_STR_VBRK-KNUMV
                                                   KPOSN = L_STR_VBRK-POSNR KSCHL = 'JOUG' BINARY SEARCH.
    IF SY-SUBRC = 0.
      L_STR_FINAL-TAX_VALUE = L_STR_KONV-KAWRT.
      L_STR_FINAL-SGST_AMOUNT      = L_STR_KONV-KWERT.
      L_STR_FINAL-SGST_RATE            = ( L_STR_FINAL-SGST_RATE + L_STR_KONV-KBETR ).
    ENDIF.

    " LOGIC FOR PRICEPERQTY

    READ TABLE L_TAB_KONV INTO L_STR_KONV WITH KEY KNUMV = L_STR_VBRK-KNUMV
                                                   KPOSN = L_STR_VBRK-POSNR KSCHL = 'ZP01' BINARY SEARCH.
    IF SY-SUBRC = 0.
*        l_str_final-priceperqty     = l_str_konv-kbetr.
    ENDIF.

    READ TABLE L_TAB_KONV INTO L_STR_KONV WITH KEY KNUMV = L_STR_VBRK-KNUMV
                                                   KPOSN = L_STR_VBRK-POSNR KSCHL = 'ZMR1' BINARY SEARCH.
    IF SY-SUBRC = 0.
      IF L_STR_FINAL-TAX_VALUE IS INITIAL.
        L_STR_FINAL-TAX_VALUE     = L_STR_KONV-KWERT.
      ENDIF.
    ENDIF.

    READ TABLE L_TAB_KONV INTO L_STR_KONV WITH KEY KNUMV = L_STR_VBRK-KNUMV
                                                   KPOSN = L_STR_VBRK-POSNR KSCHL = 'ZSPC' BINARY SEARCH.
    IF SY-SUBRC = 0.
**        l_str_final-priceperqty     = l_str_konv-kbetr.
    ENDIF.
    IF L_STR_KNA1-LAND1 = 'IN'.
      READ TABLE L_TAB_KONV INTO L_STR_KONV WITH KEY KNUMV = L_STR_VBRK-KNUMV
                                                     KPOSN = L_STR_VBRK-POSNR KSCHL = 'ZPRS' BINARY SEARCH.
      IF SY-SUBRC = 0.
        IF L_STR_FINAL-TAX_VALUE IS INITIAL.
          L_STR_FINAL-TAX_VALUE    = L_STR_KONV-KWERT.
        ENDIF.
      ENDIF.
    ENDIF.

    READ TABLE L_TAB_KONV INTO L_STR_KONV WITH KEY KNUMV = L_STR_VBRK-KNUMV
                                                   KPOSN = L_STR_VBRK-POSNR KSCHL = 'ZART' BINARY SEARCH.
    IF SY-SUBRC = 0.
*        l_str_final-priceperqty     = l_str_konv-kbetr.
    ENDIF.
    IF L_STR_KNA1-LAND1 NE 'IN'.
      READ TABLE L_TAB_KONV INTO L_STR_KONV WITH KEY KNUMV = L_STR_VBRK-KNUMV
                                                     KPOSN = L_STR_VBRK-POSNR KSCHL = 'DIFF' BINARY SEARCH.
      IF SY-SUBRC = 0.
        IF L_STR_FINAL-TAX_VALUE IS INITIAL.
          L_STR_FINAL-TAX_VALUE    = L_STR_KONV-KAWRT.
        ENDIF.
      ENDIF.
    ENDIF.

*    ENDIF.


**********************************************************************
    READ TABLE L_TAB_VBPA INTO L_STR_VBPA WITH KEY VBELN = L_STR_VBRK-VBELN BINARY SEARCH.
    IF SY-SUBRC = 0.
      READ TABLE L_TAB_KNA1 INTO L_STR_KNA1 WITH KEY KUNNR = L_STR_VBPA-KUNNR BINARY SEARCH.
      IF SY-SUBRC = 0.
        L_STR_FINAL-BILL_TO_GSTIN = L_STR_KNA1-STCD3.
        L_STR_FINAL-BILL_TO_CITY  = L_STR_KNA1-ORT01.
*        l_str_final-bill_to_district = l_str_kna1-ort01.
        L_STR_FINAL-BILL_TO_POST_CODE = L_STR_KNA1-PSTLZ.
        L_STR_FINAL-BILL_TO_NAME   = L_STR_KNA1-NAME1.
        READ TABLE L_TAB_ZGST_STATE_CODE INTO L_STR_ZGST_STATE_CODE WITH KEY REGIO = L_STR_KNA1-REGIO BINARY SEARCH.
        IF SY-SUBRC = 0.
          L_STR_FINAL-POS            = L_STR_ZGST_STATE_CODE-EWAY_STATE_CODE.
          L_STR_FINAL-BILL_TO_STATE = L_STR_ZGST_STATE_CODE-EWAY_STATE_CODE.
        ENDIF.
*        l_str_final-location_gstin            = l_str_kna1-stcd3.
        READ TABLE L_TAB_J_1BBRANCH INTO L_STR_J_1BBRANCH WITH KEY BUKRS = L_STR_VBRK-BUKRS  BRANCH = L_STR_VBRK-BUPLA .
        IF SY-SUBRC = 0.
          L_STR_FINAL-LOCATION_GSTIN = L_STR_J_1BBRANCH-GSTIN.
*          l_str_final-location_legal_name  = l_str_j_1bbranch-name.
          L_STR_FINAL-BILL_FROM_GSTIN = L_STR_J_1BBRANCH-GSTIN.
*          l_str_out_final-billfromtradename = l_str_j_1bbranch-name.
        ENDIF.

        READ TABLE L_TAB_KNA1_TT INTO L_STR_KNA1_TT WITH KEY KUNNR = L_STR_VBPA_TT-KUNNR BINARY SEARCH.
        IF SY-SUBRC = 0.
          L_STR_FINAL-SHIP_TO_GSTIN = L_STR_KNA1_TT-STCD3.
          L_STR_FINAL-SHIP_TO_CITY = L_STR_KNA1_TT-ORT01.
*          l_str_final-SHIP_TO_STATE = l_str_kna1_tt-ort01.
        ENDIF.

        READ TABLE L_TAB_ZGST_STATE_CODE INTO L_STR_ZGST_STATE_CODE WITH KEY REGIO = L_STR_KNA1_TT-REGIO BINARY SEARCH.
        IF SY-SUBRC = 0.
          L_STR_FINAL-SHIP_TO_STATE = L_STR_ZGST_STATE_CODE-EWAY_STATE_CODE. .
        ENDIF.

        L_STR_FINAL-DOCUMENT_PURPOSE = 'GST'.
        L_STR_FINAL-SUPPLY_TYPE                    = 'O'.
*   l_str_final-TRANSACTION_TYPE = 'B2B'.


        READ TABLE L_TAB_T001W1 INTO L_STR_T001W1 WITH KEY WERKS = L_STR_VBRK-WERKS.
        IF SY-SUBRC = 0.
*          l_str_final-bill_from_city        = l_str_t001w1-ort01.
*        l_str_out_final-billfromstatecode   = l_str_t001w1-regio.
          L_STR_FINAL-BILL_FROM_POST_CODE = L_STR_T001W1-PSTLZ.

          READ TABLE L_TAB_ZGST_STATE_CODE INTO L_STR_ZGST_STATE_CODE WITH KEY REGIO = L_STR_T001W1-REGIO BINARY SEARCH.
          IF SY-SUBRC = 0.
            L_STR_FINAL-BILL_FROM_STATE = L_STR_ZGST_STATE_CODE-EWAY_STATE_CODE.
          ENDIF.
        ENDIF.
*
*   READ TABLE l_tab_doctype INTO l_str_doctype WITH KEY
*   l_str_out_final-documenttype                  = l_str_temp-doctype.
*      ELSE.
*        CLEAR: l_str_final,
*               l_str_cust_gstin,
*               l_str_t001w,
*               l_str_ven_gstin,
*               l_str_vbrk.
**               l_str_bus_id, l_str_bus_unit_id.
*        CLEAR:l_str_final,
**              l_str_zgst_state_code,
*              l_str_kna1,
*              l_str_vbpa,
*              l_str_konv.
*        CONTINUE.
      ENDIF.

**      READ TABLE l_tab_cust_gstin INTO l_str_cust_gstin WITH KEY kunnr = l_str_vbrk-kunrg.
**      IF sy-subrc = 0.
**        READ TABLE l_tab_t001w1 INTO l_str_t001w1 WITH KEY werks = l_str_vbrk-werks.
**        IF sy-subrc = 0.
**          READ TABLE l_tab_ven_gstin INTO l_str_ven_gstin WITH KEY lifnr = l_str_t001w1-werks.
**          IF sy-subrc = 0.
**            IF l_str_cust_gstin-land1 <> l_str_ven_gstin-land1.
**              l_str_final-export_type = 'Y'.
**              DATA(e_type) = l_str_final-export_type.
**            ENDIF.
**          ENDIF.
**        ENDIF.
**      ENDIF.

**      l_str_final-invoice_value      = ( ( l_str_vbrk-netwr + l_str_vbrk-mwsbk ) * l_str_vbrk-kurrf ).
*      l_str_final-taxable_value      = ( ( l_str_vbrk-i_netwr ) * l_str_vbrk-kurrf ).

**      IF e_type = 'Y'..
*      IF l_str_final-cgst_amount IS INITIAL AND l_str_final-igst_amount IS INITIAL AND
*           l_str_final-sgst_amount  IS INITIAL AND l_str_final-cess_amt IS INITIAL.
      IF L_STR_VBRK-WAERK NE 'INR' OR L_STR_KNA1-LAND1 NE 'IN'.
        CLEAR L_STR_FINAL-IGST_AMOUNT.
      ENDIF.
      IF ( L_STR_VBRK-WAERK NE 'INR' AND L_STR_FINAL-IGST_AMOUNT   IS NOT INITIAL ) OR (  L_STR_KNA1-LAND1 NE 'IN'  AND L_STR_FINAL-IGST_AMOUNT  IS NOT INITIAL ) .

        L_STR_FINAL-SUB_SUPPLY_TYPE = 'EXPWP'.
      ELSE.
        IF ( L_STR_VBRK-WAERK NE 'INR' AND L_STR_FINAL-IGST_AMOUNT IS  INITIAL ) OR (  L_STR_KNA1-LAND1 NE 'IN'  AND L_STR_FINAL-IGST_AMOUNT  IS  INITIAL ).
          L_STR_FINAL-SUB_SUPPLY_TYPE = 'EXPWOP'.
        ELSE.
          L_STR_FINAL-SUB_SUPPLY_TYPE = ' '.
        ENDIF.
      ENDIF.

**      ENDIF.

**      IF e_type = 'Y'.
      L_STR_FINAL-REV_CHARGES           = 'N'.
*      l_str_final-custom2            = l_str_kna1-kunnr.
      L_STR_FINAL-CUSTOM3            = L_STR_KNA1-NAME1.
      REPLACE ALL OCCURRENCES OF REGEX '[^[:alnum:][:space:]]' IN L_STR_FINAL-CUSTOM3 WITH SPACE.
      L_STR_FINAL-WERKS              = L_STR_VBRK-WERKS.
*      l_str_final-branch             = l_str_vbrk-bupla.
      L_STR_FINAL-BUKRS              = L_STR_VBRK-BUKRS.


**        IF l_str_vbrk-fkdat LT '20181101'.                            "KS Suggested By Mukesh Asopa
**          l_str_final-invoice_no            = l_str_vbrk-vbeln.
**          l_str_final-custom1               = l_str_vbrk-xblnr.
**        ELSE.
      L_STR_FINAL-DOCUMENT_NO            = L_STR_VBRK-XBLNR.
      L_STR_FINAL-CUSTOM1               = L_STR_VBRK-VBELN.
*        ENDIF.

*      l_str_final-fkart              = l_str_vbrk-fkart.

*      l_str_final-invoice_item       = l_str_vbrk-posnr.
      L_STR_FINAL-DOCUMENT_DATE       = L_STR_VBRK-FKDAT.

*      lv_rate_str = l_str_final-rate.

*      CONCATENATE l_str_final-branch l_str_final-location_gstin l_str_final-document_no l_str_final-product_name l_str_final-hsn lv_rate_str INTO l_str_final-l_key.


**        APPEND l_str_final TO l_tab_final.
**        CLEAR:l_str_final,
**              l_var_skip,
**              l_str_kna1,
**              l_str_konv.
*              l_str_zgst_state_code.
**      ENDIF.
**      CLEAR e_type.

      L_STR_FINAL-REV_CHARGES           = 'N'.
      L_STR_FINAL-CUSTOM2            = L_STR_KNA1-KUNNR.
      L_STR_FINAL-CUSTOM3            = L_STR_KNA1-NAME1.
      REPLACE ALL OCCURRENCES OF REGEX '[^[:alnum:][:space:]]' IN L_STR_FINAL-CUSTOM3 WITH SPACE.
      L_STR_FINAL-WERKS              = L_STR_VBRK-WERKS.
*      l_str_final-branch             = l_str_vbrk-bupla.
      L_STR_FINAL-BUKRS              = L_STR_VBRK-BUKRS.


**      IF l_str_vbrk-fkdat LT '20181101'.
**        l_str_final-invoice_no            = l_str_vbrk-vbeln.
**        l_str_final-custom1               = l_str_vbrk-xblnr.
**      ELSE.
      L_STR_FINAL-DOCUMENT_NO            = L_STR_VBRK-XBLNR.
      L_STR_FINAL-CUSTOM1               = L_STR_VBRK-VBELN.
      L_STR_FINAL-CUSTOM2               = L_STR_VBRK-FKART.
      L_STR_FINAL-CUSTOM5 = L_STR_VBRK-POSNR.

**      ENDIF.

*      l_str_final-fkart              = l_str_vbrk-fkart.

*      l_str_final-invoice_item       = l_str_vbrk-posnr.
      L_STR_FINAL-DOCUMENT_DATE       = L_STR_VBRK-FKDAT.

*      l_str_final-TAX_VALUE      = ( ( l_str_final-quantity * l_str_final-priceperqty ) * l_str_vbrk-kurrf ).
*      l_str_final-invoice_value      = ( ( l_str_final-taxable_value + l_str_vbrk-mwsbk ) * l_str_vbrk-kurrf ).
* l_str_final-TAX_VALUE      = (  l_str_final-quantity   * l_str_vbrk-kurrf ).


      READ TABLE L_TAB_VBPA INTO L_STR_VBPA WITH KEY VBELN = L_STR_VBRK-VBELN  PARVW = 'RE'.
      READ TABLE  L_TAB_VBPA_TT INTO L_STR_VBPA_TT WITH KEY VBELN = L_STR_VBRK-VBELN PARVW = 'WE'.
      IF L_STR_VBPA IS NOT INITIAL AND L_STR_VBPA_TT IS NOT INITIAL.
        IF L_STR_VBPA-KUNNR EQ L_STR_VBPA_TT-KUNNR.
          L_STR_FINAL-TRANSACTION_TYPE = 'REG'.
        ELSE.
          L_STR_FINAL-TRANSACTION_TYPE = 'SHP'.
        ENDIF.
      ENDIF.
*

      IF L_STR_VBRK-FKART = 'ZF'.
        L_STR_FINAL-TRANSACTION_TYPE = 'B2C'.
      ELSE.

        L_STR_FINAL-TRANSACTION_TYPE = 'B2B'.
      ENDIF.
*  l_str_final-document_type = 'INV'.
      L_STR_FINAL-DOCUMENT_VALUE = L_STR_FINAL-TAX_VALUE + L_STR_FINAL-IGST_AMOUNT + L_STR_FINAL-CGST_AMOUNT + L_STR_FINAL-SGST_AMOUNT.

      L_STR_FINAL-ELIG_ITC = 'NO'.

      IF L_STR_VBRK-WAERK NE 'INR'.
        L_STR_FINAL-TAX_VALUE = L_STR_FINAL-TAX_VALUE * L_STR_VBRK-KURRF.
        L_STR_FINAL-DOCUMENT_VALUE = L_STR_FINAL-DOCUMENT_VALUE * L_STR_VBRK-KURRF.
        L_STR_FINAL-IGST_AMOUNT = L_STR_FINAL-IGST_AMOUNT * L_STR_VBRK-KURRF.

      ENDIF.
      IF L_STR_FINAL-SUB_SUPPLY_TYPE = 'EXPWP'.
        L_STR_FINAL-BILL_TO_STATE = '99'.
      ENDIF.
      IF L_STR_FINAL-SUB_SUPPLY_TYPE IS INITIAL.
        L_STR_FINAL-SUB_SUPPLY_TYPE = 'SUP'.
      ENDIF.
      IF L_STR_FINAL-DOCUMENT_TYPE = 'CRN' OR L_STR_FINAL-DOCUMENT_TYPE = 'DRN'.
        L_STR_FINAL-REFERENCE_DOC_TYPE = 'INV'.
      ENDIF.


      L_STR_FINAL-ELIG_ITC = 'NO'.
      IF L_STR_FINAL-UOM = 'MLT'.
        L_STR_FINAL-QUANTITY = L_STR_FINAL-QUANTITY * 1000.
      ENDIF.

      L_STR_FINAL-DOC_STATUS = 'Draft'.
      APPEND L_STR_FINAL TO GT_FINAL_OUTWARD.

      CLEAR:L_STR_FINAL,
            L_VAR_SKIP,
            L_STR_KNA1,
            L_STR_KONV.
*            l_str_zgst_state_code.
    ENDIF.
    CLEAR:L_STR_FINAL,
          L_VAR_SKIP,
          L_STR_KNA1,
          L_STR_KONV.
*          l_str_zgst_state_code,
*          e_type.
  ENDLOOP.

  DELETE GT_FINAL_OUTWARD WHERE ( CGST_RATE IS NOT INITIAL AND SGST_RATE IS NOT INITIAL ) AND ( CGST_AMOUNT IS INITIAL AND SGST_AMOUNT IS INITIAL ).
*DELETE GT_FINAL_OUTWARD WHERE CUSTOM2 = 'ZSTO' AND cgst_amount IS INITIAL AND sgst_amount IS INITIAL.
  DELETE GT_FINAL_OUTWARD WHERE CUSTOM2 = 'ZSTO' AND IGST_AMOUNT IS INITIAL .
*  direct fi code

  CLEAR : L_TAB_FINAL.
  SORT L_TAB_KNA1 BY KUNNR.
  SORT L_TAB_BKPF_FI BY BUKRS BELNR GJAHR.
  SORT L_TAB_BKPF_DFI BY BUKRS BELNR GJAHR.
  SORT L_TAB_BSEG_DFI BY BUKRS BELNR GJAHR.
  SORT L_TAB_BSET_DFI BY BUKRS BELNR GJAHR.
  SORT L_TAB_BSEG_DFI_SGTXT BY BUKRS BELNR GJAHR .
  SORT L_TAB_BKPF_DFI_SGTXT BY BUKRS BELNR GJAHR ."xblnr gjahr.
  SORT L_TAB_BSEG_DFI_KIDNO BY BUKRS BELNR GJAHR .
  SORT L_TAB_BKPF_DFI_KIDNO BY BUKRS BELNR GJAHR ."xblnr gjahr.
  SORT L_TAB_BSEG BY BUKRS BELNR GJAHR BUZEI.
  SORT L_TAB_BSET BY BUKRS BELNR GJAHR BUZEI TXGRP.
  SORT L_TAB_J_1BBRANCH BY BRANCH.
  SORT L_TAB_BSEG_TEMP  BY BUKRS BELNR GJAHR.
*  sort l_tab_bseg_dfi_lifnr by lifnr name1 regio.
  LOOP AT L_TAB_BKPF_DFI INTO L_STR_BKPF_DFI.
    L_STR_FINAL-HSN = L_STR_BSEG_DFI-HSN_SAC.
    L_STR_FINAL-PRODUCT_NAME = L_STR_BSEG_DFI-MATNR.

    READ TABLE L_TAB_BSEG_DFI INTO L_STR_BSEG_DFI WITH KEY BUKRS = L_STR_BKPF_DFI-BUKRS
                                                   BELNR = L_STR_BKPF_DFI-BELNR
                                                   GJAHR = L_STR_BKPF_DFI-GJAHR ."BINARY SEARCH.
    IF SY-SUBRC = 0.
      L_STR_FINAL-DOCUMENT_NO          = L_STR_BKPF_DFI-XBLNR.
      L_STR_FINAL-DOCUMENT_DATE        = L_STR_BKPF_DFI-BUDAT.
*        l_str_final-invoice_value       = l_str_bseg-wrbtr.

      MOVE L_STR_BKPF_DFI-BELNR TO L_STR_FINAL-CUSTOM1.
      MOVE L_STR_BKPF_DFI-BLART TO L_STR_FINAL-CUSTOM2.
    ENDIF.

    READ TABLE L_TAB_BSEG_DFI_HSN INTO L_STR_BSEG_DFI_HSN WITH KEY BUKRS = L_STR_BKPF_DFI-BUKRS
                                                                  BELNR = L_STR_BKPF_DFI-BELNR
                                                   GJAHR = L_STR_BKPF_DFI-GJAHR .
    IF SY-SUBRC = 0.
      L_STR_FINAL-HSN = L_STR_BSEG_DFI_HSN-HSN_SAC.
    ENDIF.

    READ TABLE L_TAB_DOCTYPE1 INTO L_STR_DOCTYPE1 WITH KEY SUPPLYTYPE = 'O'
                                                        BLART = L_STR_FINAL-CUSTOM2.
    IF SY-SUBRC = 0.
      L_STR_FINAL-DOCUMENT_TYPE = L_STR_DOCTYPE1-DOCTYPE.
    ENDIF.
    READ TABLE L_TAB_BSEG_DFI_KUNNR INTO L_STR_BSEG_DFI_KUNNR WITH KEY BUKRS = L_STR_BSEG_DFI-BUKRS
                                                            BELNR = L_STR_BSEG_DFI-BELNR
                                                            GJAHR = L_STR_BSEG_DFI-GJAHR ."BINARY SEARCH.
    IF SY-SUBRC = 0.
      READ TABLE L_TAB_KNA1_DFI INTO L_STR_KNA1_DFI WITH KEY KUNNR = L_STR_BSEG_DFI_KUNNR-KUNNR BINARY SEARCH.
      IF L_STR_KNA1_DFI-STCD3 IS NOT INITIAL.
        L_STR_FINAL-BILL_TO_GSTIN = L_STR_KNA1_DFI-STCD3.
        L_STR_FINAL-BILL_TO_CITY = L_STR_KNA1_DFI-ORT01.
*          l_str_final-kunrg = l_str_kna1_fi-kunnr.
      ELSE.
        CLEAR : L_STR_BKPF_FI."l_str_bseg
      ENDIF.
    ENDIF.

*    FOR KG TYPE vendor creditMEMO
    READ TABLE L_TAB_BSEG_DFI_LIFNR INTO L_STR_BSEG_DFI_LIFNR WITH KEY BUKRS = L_STR_BSEG_DFI-BUKRS
                                                          BELNR = L_STR_BSEG_DFI-BELNR
                                                          GJAHR = L_STR_BSEG_DFI-GJAHR ."BINARY SEARCH.
    IF SY-SUBRC = 0.
      READ TABLE L_TAB_LFA1_DFI INTO L_STR_LFA1_DFI WITH KEY LIFNR = L_STR_BSEG_DFI_LIFNR-LIFNR BINARY SEARCH.
      IF L_STR_LFA1_DFI-STCD3 IS NOT INITIAL.
        L_STR_FINAL-BILL_TO_GSTIN = L_STR_LFA1_DFI-STCD3.
        L_STR_FINAL-BILL_TO_CITY = L_STR_LFA1_DFI-ORT01.
*          l_str_final-kunrg = l_str_kna1_fi-kunnr.
      ELSE.
        CLEAR : L_STR_BKPF_FI."l_str_bseg
      ENDIF.
    ENDIF.

    READ TABLE L_TAB_ZGST_STATE_CODE INTO L_STR_ZGST_STATE_CODE WITH KEY REGIO = L_STR_KNA1_DFI-REGIO BINARY SEARCH.
    IF SY-SUBRC = 0.
      L_STR_FINAL-POS            = L_STR_ZGST_STATE_CODE-EWAY_STATE_CODE.
      L_STR_FINAL-BILL_TO_STATE = L_STR_ZGST_STATE_CODE-EWAY_STATE_CODE.
    ENDIF.
    IF L_STR_FINAL-POS IS INITIAL.
      IF L_STR_FINAL-DOCUMENT_TYPE = 'DRN' AND L_STR_BKPF_DFI-BLART = 'KG'.
        READ TABLE L_TAB_ZGST_STATE_CODE INTO L_STR_ZGST_STATE_CODE WITH KEY REGIO = L_STR_LFA1_DFI-REGIO BINARY SEARCH.
        IF SY-SUBRC = 0.
          L_STR_FINAL-POS            = L_STR_ZGST_STATE_CODE-EWAY_STATE_CODE.
          L_STR_FINAL-BILL_TO_STATE = L_STR_ZGST_STATE_CODE-EWAY_STATE_CODE.
        ENDIF.
      ENDIF.
    ENDIF.

*        l_str_final-location_gstin            = l_str_kna1-stcd3.
    READ TABLE L_TAB_J_1BBRANCH INTO L_STR_J_1BBRANCH WITH KEY BUKRS = L_STR_BSEG_DFI-BUKRS  BRANCH = L_STR_BSEG_DFI-BUPLA .
    IF SY-SUBRC = 0.
      L_STR_FINAL-LOCATION_GSTIN = L_STR_J_1BBRANCH-GSTIN.
*      l_str_final-location_legal_name  = l_str_j_1bbranch-name.
      L_STR_FINAL-BILL_FROM_GSTIN = L_STR_J_1BBRANCH-GSTIN.
*          l_str_out_final-billfromtradename = l_str_j_1bbranch-name.
    ENDIF.

    READ TABLE L_TAB_T001W1 INTO L_STR_T001W1 WITH KEY WERKS = L_STR_BSEG_DFI-WERKS.
    IF SY-SUBRC = 0.
*      l_str_final-bill_from_city        = l_str_t001w1-ort01.
*        l_str_out_final-billfromstatecode   = l_str_t001w1-regio.
      L_STR_FINAL-BILL_FROM_POST_CODE = L_STR_T001W1-PSTLZ.

      READ TABLE L_TAB_ZGST_STATE_CODE INTO L_STR_ZGST_STATE_CODE WITH KEY REGIO = L_STR_T001W1-REGIO BINARY SEARCH.
      IF SY-SUBRC = 0.
        L_STR_FINAL-BILL_FROM_STATE = L_STR_ZGST_STATE_CODE-EWAY_STATE_CODE.
      ENDIF.
    ENDIF.
*logic for zd and ze type
    IF L_STR_BKPF_DFI-BLART = 'ZD'.
      CLEAR L_STR_FINAL-REFERENCE_DOC_NO.
      CLEAR L_STR_FINAL-REFERENCE_DOC_DATE.
      READ TABLE L_TAB_BSEG_DFI_SGTXT INTO L_STR_BSEG_DFI_SGTXT WITH KEY BUKRS = L_STR_BKPF_DFI-BUKRS
                                                                         BELNR = L_STR_BKPF_DFI-BELNR
                                                                         GJAHR = L_STR_BKPF_DFI-GJAHR BINARY SEARCH.
      IF SY-SUBRC = 0.
        L_STR_FINAL-REFERENCE_DOC_NO = L_STR_BSEG_DFI_SGTXT-SGTXT+4(12).
        READ TABLE L_TAB_BKPF_DFI_SGTXT INTO L_STR_BKPF_DFI_SGTXT WITH KEY XBLNR = L_STR_FINAL-REFERENCE_DOC_NO
                                                                           GJAHR = L_STR_BSEG_DFI_SGTXT-GJAHR.
        IF SY-SUBRC = 0.
          L_STR_FINAL-REFERENCE_DOC_DATE = L_STR_BKPF_DFI_SGTXT-BUDAT.
        ENDIF.
      ENDIF.

      L_STR_FINAL-DOCUMENT_NO = L_STR_BKPF_DFI-XBLNR.
      L_STR_FINAL-DOCUMENT_DATE = L_STR_BKPF_DFI-BUDAT.
    ENDIF.

    IF L_STR_BKPF_DFI-BLART = 'ZE'.
      CLEAR L_STR_FINAL-REFERENCE_DOC_NO.
      CLEAR L_STR_FINAL-REFERENCE_DOC_DATE.
      READ TABLE L_TAB_BSEG_DFI_SGTXT1 INTO L_STR_BSEG_DFI_SGTXT1 WITH KEY BUKRS = L_STR_BKPF_DFI-BUKRS
                                                                         BELNR = L_STR_BKPF_DFI-BELNR
                                                                         GJAHR = L_STR_BKPF_DFI-GJAHR BINARY SEARCH.
      IF SY-SUBRC = 0.
        L_STR_FINAL-REFERENCE_DOC_NO = L_STR_BSEG_DFI_SGTXT1-SGTXT+4(12).
        READ TABLE L_TAB_BKPF_DFI_KIDNO INTO L_STR_BKPF_DFI_KIDNO WITH KEY XBLNR = L_STR_FINAL-REFERENCE_DOC_NO
                                                                           GJAHR = L_STR_BSEG_DFI_SGTXT1-GJAHR.
        IF SY-SUBRC = 0.
          L_STR_FINAL-REFERENCE_DOC_DATE = L_STR_BKPF_DFI_KIDNO-BUDAT.
        ENDIF.
      ENDIF.

      L_STR_FINAL-DOCUMENT_NO = L_STR_BKPF_DFI-XBLNR.
      L_STR_FINAL-DOCUMENT_DATE = L_STR_BKPF_DFI-BUDAT.
    ENDIF.



    IF L_STR_FINAL-CUSTOM3 IS INITIAL .
      CONCATENATE L_STR_KNA1_FI-KUNNR L_STR_KNA1_FI-NAME1 INTO L_STR_FINAL-CUSTOM3 SEPARATED BY SPACE.
    ENDIF.

    READ TABLE L_TAB_BSET_DFI TRANSPORTING NO FIELDS WITH KEY BUKRS = L_STR_BKPF_DFI-BUKRS
                                                          BELNR = L_STR_BKPF_DFI-BELNR
                                                          GJAHR = L_STR_BKPF_DFI-GJAHR BINARY SEARCH.
*                                                            buzei = l_str_bseg_dfi-txgrp BINARY SEARCH.
    IF SY-SUBRC = 0.
*        else.
*          CONTINUE.

***Gst conditions******************************************************************************
      L_VAR_TABIX = SY-TABIX.
      LOOP AT L_TAB_BSET_DFI INTO L_STR_BSET_DFI FROM L_VAR_TABIX WHERE BUKRS = L_STR_BKPF_DFI-BUKRS AND
                                                                BELNR = L_STR_BKPF_DFI-BELNR AND
                                                                GJAHR = L_STR_BKPF_DFI-GJAHR .
        "   buzei = l_str_bseg-buzei. "txgrp
*                                                                    txgrp = l_str_bseg_dfi-txgrp.
        CASE L_STR_BSET_DFI-KSCHL.
          WHEN 'JOCG' OR 'JICG' .
            L_STR_FINAL-TAX_VALUE = L_STR_BSET_DFI-FWBAS.
            IF L_STR_BSET_DFI-SHKZG = 'H'.
*                l_str_final-cgst_amount = l_str_final-cgst_amount - l_str_bset_dfi-fwste.
*              ELSE.
*                l_str_final-cgst_amount = l_str_final-cgst_amount + l_str_bset_dfi-fwste.
              L_STR_FINAL-CGST_AMOUNT =   L_STR_BSET_DFI-FWSTE * -1.
            ELSE.
              L_STR_FINAL-CGST_AMOUNT =   L_STR_BSET_DFI-FWSTE .
            ENDIF.
            IF L_STR_FINAL-CGST_AMOUNT LT '0'.
              L_STR_FINAL-CGST_AMOUNT = L_STR_FINAL-CGST_AMOUNT * -1.
            ENDIF.
*              l_str_final-cgst_rate = l_str_final-cgst_rate + ( l_str_bset_dfi-kbetr /  10 ).

            L_STR_FINAL-CGST_RATE =  ( L_STR_BSET_DFI-KBETR /  10 ).
          WHEN 'JOIG' OR 'JIIG'  .
            L_STR_FINAL-TAX_VALUE =   L_STR_BSET_DFI-FWBAS.
            IF L_STR_BSET_DFI-SHKZG = 'H'.
*                l_str_final-igst_amount = l_str_final-igst_amount - l_str_bset_dfi-fwste.
*              ELSE.
*                l_str_final-igst_amount = l_str_final-igst_amount + l_str_bset_dfi-fwste.
*              ENDIF.
              L_STR_FINAL-IGST_AMOUNT =   L_STR_BSET_DFI-FWSTE * -1..
            ELSE.
              L_STR_FINAL-IGST_AMOUNT =  L_STR_BSET_DFI-FWSTE.
            ENDIF.
            IF L_STR_FINAL-IGST_AMOUNT LT '0'.
              L_STR_FINAL-IGST_AMOUNT = L_STR_FINAL-IGST_AMOUNT * -1.
            ENDIF.
*              l_str_final-igst_rate = l_str_final-igst_rate + ( l_str_bset_dfi-kbetr /  10 ).
            L_STR_FINAL-IGST_RATE =  L_STR_BSET_DFI-KBETR /  10 .
          WHEN 'JOSG' OR 'JOUG' OR 'JISG' OR 'JIUG'.
            L_STR_FINAL-TAX_VALUE = L_STR_BSET_DFI-FWBAS.
            IF L_STR_BSET_DFI-SHKZG = 'H'.
*                l_str_final-sgst_amount = l_str_final-sgst_amount - l_str_bset_dfi-fwste.
*              ELSE.
*                l_str_final-sgst_amount = l_str_final-sgst_amount + l_str_bset_dfi-fwste.
*              ENDIF.
              L_STR_FINAL-SGST_AMOUNT =   L_STR_BSET_DFI-FWSTE * -1.
            ELSE.
              L_STR_FINAL-SGST_AMOUNT =   L_STR_BSET_DFI-FWSTE.
            ENDIF.
            IF L_STR_FINAL-SGST_AMOUNT LT '0'.
              L_STR_FINAL-SGST_AMOUNT = L_STR_FINAL-SGST_AMOUNT * -1.
            ENDIF.
*              l_str_final-sgst_rate = l_str_final-sgst_rate + ( l_str_bset_dfi-kbetr /  10 ).
            L_STR_FINAL-SGST_RATE =  L_STR_BSET_DFI-KBETR /  10 .
        ENDCASE.
        CLEAR : L_STR_BSET_DFI.
*              l_str_final-branch = l_str_bseg-bupla.
      ENDLOOP.

    ELSE.
      CONTINUE.
    ENDIF.
*    ENDLOOP.


*endif.

    READ TABLE L_TAB_T001W INTO L_STR_T001W WITH KEY WERKS = L_STR_BSEG_DFI-WERKS BINARY SEARCH.
    L_STR_FINAL-BUKRS = L_STR_BSEG_DFI-BUKRS.
*****      l_str_final-rev_charge          = 'N'.

*      IF l_str_final-cgst_amount EQ '0.00' OR l_str_final-cgst_amount EQ '0'.
*        CLEAR l_str_final-cgst_amount.
*      ENDIF.
*
*      IF l_str_final-cgst_amount EQ '0.00' OR l_str_final-intr_tax_amt EQ '0'.
*        CLEAR l_str_final-intr_tax_amt.
*      ENDIF.
*
*      IF l_str_final-intr_tax_amt EQ '0.00' OR l_str_final-intr_tax_amt EQ '0'.
*        CLEAR l_str_final-intr_tax_amt.
*      ENDIF.

    READ TABLE L_TAB_BKPF_FI INTO L_STR_BKPF_FI WITH KEY BUKRS = L_STR_BSEG_DFI-BUKRS
                                                   BELNR = L_STR_BSEG_DFI-BELNR
                                                   GJAHR = L_STR_BSEG_DFI-GJAHR BINARY SEARCH.

*      if l_str_final-SUB_SUPPLY_TYPE = 'EXPWP'.
    L_STR_FINAL-DOCUMENT_VALUE = L_STR_FINAL-TAX_VALUE + L_STR_FINAL-CGST_AMOUNT + L_STR_FINAL-IGST_AMOUNT + L_STR_FINAL-SGST_AMOUNT.
*        ENDIF.

    IF L_STR_BKPF_FI-WAERS NE 'INR' AND L_STR_FINAL-DOCUMENT_TYPE = 'EXPWOP'.
      L_STR_FINAL-DOCUMENT_VALUE = L_STR_BKPF_FI-KURSF * L_STR_FINAL-DOCUMENT_VALUE.
      L_STR_FINAL-TAX_VALUE = L_STR_FINAL-DOCUMENT_VALUE.
    ENDIF.
    L_STR_FINAL-REV_CHARGES = 'N'.
    L_STR_FINAL-CUSTOM3 = L_STR_KNA1_DFI-NAME1.
    L_STR_FINAL-CUSTOM4 = L_STR_BSEG_DFI-BELNR.

    L_STR_FINAL-DOCUMENT_PURPOSE = 'GST'.
    L_STR_FINAL-SUPPLY_TYPE = 'O'.
    L_STR_FINAL-TRANSACTION_TYPE = 'Regular'.

    IF L_STR_FINAL-SUB_SUPPLY_TYPE IS INITIAL.
      L_STR_FINAL-SUB_SUPPLY_TYPE = 'SUP'.
    ENDIF.

    IF L_STR_FINAL-DOCUMENT_TYPE = 'CRN' OR L_STR_FINAL-DOCUMENT_TYPE = 'DBN'.
      L_STR_FINAL-REFERENCE_DOC_TYPE = 'INV'.
    ENDIF.

*    *   zd and ze logic for refno and refdate
    IF L_STR_BKPF_DFI-BLART  ='ZD'.
    ENDIF.
    L_STR_FINAL-CUSTOM10 = L_STR_BSEG_DFI-BUPLA.
    IF L_STR_FINAL-QUANTITY IS INITIAL.
      L_STR_FINAL-QUANTITY = '1.00'.
    ENDIF.
    IF L_STR_FINAL-UOM IS INITIAL.
      L_STR_FINAL-UOM = 'NOS'.
    ENDIF.
    IF L_STR_FINAL-CGST_AMOUNT IS NOT INITIAL AND L_STR_FINAL-SGST_AMOUNT IS NOT INITIAL.
      CLEAR L_STR_FINAL-IGST_AMOUNT.
    ENDIF.
    L_STR_FINAL-DOC_STATUS = 'Draft'.
    APPEND L_STR_FINAL TO GT_FINAL_OUTWARD..
    CLEAR : L_STR_FINAL , L_STR_KNA1_DFI.
  ENDLOOP.
  DELETE ADJACENT DUPLICATES FROM GT_FINAL_OUTWARD COMPARING ALL FIELDS .
  DELETE GT_FINAL_OUTWARD WHERE QUANTITY IS INITIAL.
  DELETE GT_FINAL_OUTWARD WHERE TAX_VALUE IS INITIAL AND CGST_AMOUNT IS INITIAL AND IGST_AMOUNT IS INITIAL AND CGST_AMOUNT IS INITIAL AND ( SUB_SUPPLY_TYPE NE 'EXPWOP' AND SUB_SUPPLY_TYPE NE 'EXPWP' )  .
*DELETE ADJACENT DUPLICATES FROM  GT_FINAL_OUTWARD WHERE igst_RATE = '36'.

  MOVE GT_FINAL_OUTWARD TO GT_FINAL_OUTWARD1.
  CLEAR GT_FINAL_OUTWARD.
  SORT GT_FINAL_OUTWARD1 BY LOCATION_GSTIN CUSTOM1 HSN UOM.
  LOOP AT GT_FINAL_OUTWARD1 INTO GL_FINAL_OUTWARD1.
    L_STR_FINAL-CUSTOM1 = GL_FINAL_OUTWARD1-CUSTOM1.
    L_STR_FINAL-PRODUCT_NAME = GL_FINAL_OUTWARD1-PRODUCT_NAME.
    L_STR_FINAL-HSN = GL_FINAL_OUTWARD1-HSN.
    L_STR_FINAL-UOM = GL_FINAL_OUTWARD1-UOM.
    L_STR_FINAL-PRODUCT_NAME = GL_FINAL_OUTWARD1-PRODUCT_NAME.
    L_STR_FINAL-DESCRIPTION = GL_FINAL_OUTWARD1-DESCRIPTION.
    L_STR_FINAL-CUSTOM2 = GL_FINAL_OUTWARD1-CUSTOM2.
    L_STR_FINAL-CUSTOM3 = GL_FINAL_OUTWARD1-CUSTOM3.
    L_STR_FINAL-DOCUMENT_NO = GL_FINAL_OUTWARD1-DOCUMENT_NO.
    L_STR_FINAL-DOCUMENT_DATE = GL_FINAL_OUTWARD1-DOCUMENT_DATE.
    L_STR_FINAL-LOCATION_GSTIN = GL_FINAL_OUTWARD1-LOCATION_GSTIN.
    L_STR_FINAL-LOCATION_LEGAL_NAME = GL_FINAL_OUTWARD1-LOCATION_LEGAL_NAME.
    L_STR_FINAL-DOCUMENT_PURPOSE = GL_FINAL_OUTWARD1-DOCUMENT_PURPOSE.
    L_STR_FINAL-SUPPLY_TYPE = GL_FINAL_OUTWARD1-SUPPLY_TYPE.
    L_STR_FINAL-SUB_SUPPLY_TYPE = GL_FINAL_OUTWARD1-SUB_SUPPLY_TYPE.
    L_STR_FINAL-SUB_SUPPLY_DESCRIPTION = GL_FINAL_OUTWARD1-SUB_SUPPLY_DESCRIPTION.
    L_STR_FINAL-DOCUMENT_TYPE = GL_FINAL_OUTWARD1-DOCUMENT_TYPE.
    L_STR_FINAL-SERIES_CODE = GL_FINAL_OUTWARD1-SERIES_CODE.
    L_STR_FINAL-REFERENCE_DOC_TYPE = GL_FINAL_OUTWARD1-REFERENCE_DOC_TYPE.
    L_STR_FINAL-REFERENCE_DOC_NO = GL_FINAL_OUTWARD1-REFERENCE_DOC_NO.
    L_STR_FINAL-REFERENCE_DOC_DATE = GL_FINAL_OUTWARD1-REFERENCE_DOC_DATE.

    L_STR_FINAL-TRANSACTION_TYPE = GL_FINAL_OUTWARD1-TRANSACTION_TYPE.
    L_STR_FINAL-POS = GL_FINAL_OUTWARD1-POS.
    L_STR_FINAL-BILL_FROM_GSTIN = GL_FINAL_OUTWARD1-BILL_FROM_GSTIN.

    L_STR_FINAL-BILL_FROM_OTH_PARTY_NAME = GL_FINAL_OUTWARD1-BILL_FROM_OTH_PARTY_NAME.
    L_STR_FINAL-BILL_FROM_ADDRESS1 = GL_FINAL_OUTWARD1-BILL_FROM_ADDRESS1.
    L_STR_FINAL-BILL_FROM_ADDRESS2  = GL_FINAL_OUTWARD1-BILL_FROM_ADDRESS2.
    L_STR_FINAL-BILL_FROM_CITY = GL_FINAL_OUTWARD1-BILL_FROM_CITY.
    L_STR_FINAL-BILL_FROM_STATE = GL_FINAL_OUTWARD1-BILL_FROM_STATE.
    L_STR_FINAL-BILL_FROM_POST_CODE = GL_FINAL_OUTWARD1-BILL_FROM_POST_CODE.

    L_STR_FINAL-DISPATCH_FROM_GSTIN = GL_FINAL_OUTWARD1-DISPATCH_FROM_GSTIN.
    L_STR_FINAL-DISPATCH_FROM_OTH_PARTY_NAME = GL_FINAL_OUTWARD1-DISPATCH_FROM_OTH_PARTY_NAME.

    L_STR_FINAL-DISPATCH_FROM_ADDRESS1 = GL_FINAL_OUTWARD1-DISPATCH_FROM_ADDRESS1.

    L_STR_FINAL-DISPATCH_FROM_ADDRESS2  = GL_FINAL_OUTWARD1-DISPATCH_FROM_ADDRESS2.
    L_STR_FINAL-DISPATCH_FROM_CITY = GL_FINAL_OUTWARD1-DISPATCH_FROM_CITY.
    L_STR_FINAL-DISPATCH_FROM_STATE = GL_FINAL_OUTWARD1-DISPATCH_FROM_STATE.
    L_STR_FINAL-DISPATCH_FROM_POST_CODE = GL_FINAL_OUTWARD1-DISPATCH_FROM_POST_CODE.
    L_STR_FINAL-CTIN = GL_FINAL_OUTWARD1-CTIN.
    L_STR_FINAL-EMAIL = GL_FINAL_OUTWARD1-EMAIL.
    L_STR_FINAL-MOBILE = GL_FINAL_OUTWARD1-MOBILE.
    L_STR_FINAL-BILL_TO_GSTIN = GL_FINAL_OUTWARD1-BILL_TO_GSTIN.
    L_STR_FINAL-BILL_TO_NAME = GL_FINAL_OUTWARD1-BILL_TO_NAME.
    L_STR_FINAL-BILL_TO_ADDRESS1 = GL_FINAL_OUTWARD1-BILL_TO_ADDRESS1.
    L_STR_FINAL-BILL_TO_ADDRESS2 = GL_FINAL_OUTWARD1-BILL_TO_ADDRESS2.
    L_STR_FINAL-BILL_TO_STATE  = GL_FINAL_OUTWARD1-BILL_TO_STATE.
    L_STR_FINAL-BILL_TO_CITY = GL_FINAL_OUTWARD1-BILL_TO_CITY.
    L_STR_FINAL-BILL_TO_POST_CODE = GL_FINAL_OUTWARD1-BILL_TO_POST_CODE.
    L_STR_FINAL-SHIP_TO_GSTIN = GL_FINAL_OUTWARD1-SHIP_TO_GSTIN.
    L_STR_FINAL-SHIP_TO_NAME = GL_FINAL_OUTWARD1-SHIP_TO_NAME.
    L_STR_FINAL-SHIP_TO_ADDRESS1 = GL_FINAL_OUTWARD1-SHIP_TO_ADDRESS1.
    L_STR_FINAL-SHIP_TO_ADDRESS2 = GL_FINAL_OUTWARD1-SHIP_TO_ADDRESS2.
    L_STR_FINAL-QUANTITY = L_STR_FINAL-QUANTITY + GL_FINAL_OUTWARD1-QUANTITY.
    L_STR_FINAL-TAX_VALUE = L_STR_FINAL-TAX_VALUE + GL_FINAL_OUTWARD1-TAX_VALUE.
    L_STR_FINAL-CGST_AMOUNT = L_STR_FINAL-CGST_AMOUNT + GL_FINAL_OUTWARD1-CGST_AMOUNT.
    L_STR_FINAL-IGST_AMOUNT = L_STR_FINAL-IGST_AMOUNT + GL_FINAL_OUTWARD1-IGST_AMOUNT.
    L_STR_FINAL-SGST_AMOUNT = L_STR_FINAL-SGST_AMOUNT + GL_FINAL_OUTWARD1-SGST_AMOUNT.
    L_STR_FINAL-CGST_RATE =   GL_FINAL_OUTWARD1-CGST_RATE.
    L_STR_FINAL-SGST_RATE = GL_FINAL_OUTWARD1-SGST_RATE.
    L_STR_FINAL-IGST_RATE =   GL_FINAL_OUTWARD1-IGST_RATE.
    L_STR_FINAL-DOCUMENT_VALUE = L_STR_FINAL-DOCUMENT_VALUE + GL_FINAL_OUTWARD1-DOCUMENT_VALUE.
    L_STR_FINAL-ELIG_ITC  = GL_FINAL_OUTWARD1-ELIG_ITC.
    L_STR_FINAL-REV_CHARGES = GL_FINAL_OUTWARD1-REV_CHARGES.
    L_STR_FINAL-custom10 = GL_FINAL_OUTWARD1-custom10.

*P_TO_STATE
*SHIP_TO_CITY
    AT END OF HSN.
      APPEND L_STR_FINAL TO GT_FINAL_OUTWARD.
      CLEAR L_STR_FINAL.
    ENDAT.

  ENDLOOP.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form LOCAL_FILE_GENERATION
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM LOCAL_FILE_GENERATION .

  TYPES:
    BEGIN OF TY_OUT_INVOICE,
      BUSINESS_PLACE        TYPE CHAR50,
      LOCATION_GSTIN        TYPE CHAR50,
      CUSTOMER_NAME         TYPE CHAR50,
      CUSTOMER_GSTIN        TYPE CHAR50,
      PLACE_OF_SUPPLY       TYPE CHAR50,
      GSTIN_ECOMMERCE       TYPE CHAR50,
      LEGAL_NAME            TYPE CHAR50,
      DOCUMENT_TYPE         TYPE CHAR50,
      SALES_INVOICE         TYPE CHAR50,
      SALES_INVOICE_DATE    TYPE CHAR50,
      INVOICE_TYPE          TYPE CHAR50,
      SHIPPING_BILL_NUMBER  TYPE CHAR50,
      SHIPPING_BILL_DATE    TYPE CHAR50,
      PORT_CODE             TYPE CHAR50,
      RCM_APPLICABILITY     TYPE CHAR50,
      CGST_RATE             TYPE CHAR50,
      SGST_RATE             TYPE CHAR50,
      IGST_RATE             TYPE CHAR50,
      CONSOLIDATED_RATE     TYPE CHAR50,
      CESS_RATE             TYPE CHAR50,
      TAXABLE_VALUE         TYPE CHAR50,
      DISCOUNTS             TYPE CHAR50,
      DISCOUNTS_AMOUNT      TYPE CHAR50,
      GROSS_TAXABLE_VALUE   TYPE CHAR50,
      TAX_CODE              TYPE CHAR50,
      CGST_AMOUNT           TYPE CHAR50,
      SGST_AMOUNT           TYPE CHAR50,
      IGST_AMOUNT           TYPE CHAR50,
      CESS_AMOUNT           TYPE CHAR50,
      INVOICE_VALUE         TYPE CHAR50,
      ORIGINAL_INVOICE_NO   TYPE CHAR50,
      ORIGINAL_INVOICE_DATE TYPE CHAR50,
      ORIGINAL_CD_NO        TYPE CHAR50,
      ORIGINAL_CD_DATE      TYPE CHAR50,
      HSN_CODE              TYPE CHAR50,
      UNIT_OF_MEASUREMENT   TYPE CHAR50,
      QUANTITY              TYPE CHAR50,
      RATE                  TYPE CHAR50,
      CANCELLED_DOC         TYPE CHAR50,
      DOCUMENT_NUMBER       TYPE CHAR50,
      DOCUMENT_DATE         TYPE CHAR50,
      GL_ACCOUNT            TYPE CHAR50,
      REVENUE_GL_ACCOUNT    TYPE CHAR50,
      CUSTOM_FIELD1         TYPE CHAR50,
      CUSTOM_FIELD2         TYPE CHAR50,
      CUSTOM_FIELD3         TYPE CHAR50,
      CUSTOM_FIELD4         TYPE CHAR50,
      CUSTOM_FIELD5         TYPE CHAR50,
    END OF TY_OUT_INVOICE.
***Internal Tables******************************************************
  DATA : LT_FINAL           TYPE STANDARD TABLE OF TY_OUT_INVOICE,
         LT_DATA            TYPE TABLE OF TEXT,
         LT_OUTWARD_INVOICE TYPE TRUXS_T_TEXT_DATA,
         LT_CUST_DATA       TYPE STANDARD TABLE OF ZGSP_CUST_DATA.
***Structure***********************************************************
  DATA : LS_OUT_INVOICE TYPE TY_FINAL_OUTWARD,
         LS_FINAL       TYPE TY_OUT_INVOICE,
         LS_DATE        TYPE RANGE_S_DATS,
         LS_CUST_DATA   TYPE ZGSP_CUST_DATA.


  TYPES: BEGIN OF LTY_VBRK,
           VBELN TYPE VBRK-VBELN,
           XBLNR TYPE VBRK-XBLNR,
           FKDAT TYPE VBRK-FKDAT,
         END OF  LTY_VBRK.
  TYPES: BEGIN OF TY_TITLE,
           TITLE(100),
         END OF TY_TITLE.
  DATA: LT_VBRK TYPE TABLE OF LTY_VBRK,
        LS_VBRK TYPE LTY_VBRK.
  DATA : LT_TITLE TYPE STANDARD TABLE OF TY_TITLE,
         LS_TITLE TYPE TY_TITLE.
***Variables************************************************************
  DATA : LV_PWD_LEN       TYPE I, "For finding the length of the Password, This is used when scrambling the password
         LV_HANDLE        TYPE I, "Handle for Pointing to an already connected FTP connection,used for subsequent actions on the connected FTP session
         LV_PATH          TYPE STRING, "Path that points to the FTP User's Home Directory
         LV_TSL           TYPE TIMESTAMPL, "Time Stamp
         LV_TIME_STAMP    TYPE CHAR21, "Time Stamp
         LV_BUKRS         TYPE BUKRS,
         LV_ANSWER        TYPE CHAR1,
         LV_DATE          TYPE SY-DATUM,
         L_VAR_PWD_LEN    TYPE I,
         L_VAR_HANDLE     TYPE I,
         G_VAR_RESPONSE   TYPE CHAR1,
         L_TAB_DATA       TYPE TABLE OF TEXT,
         L_VAR_PATH(100)  TYPE C,
         LV_SHIP_DATE(10) TYPE C.
***Constant*************************************************************
  CONSTANTS : L_CON_KEY       TYPE I VALUE 26101957. "Hardcoded Handler Key,This is always '26101957'
************************************************************************

  IF GT_FINAL_OUTWARD IS NOT INITIAL.
    SORT GT_FINAL_OUTWARD BY LOCATION_GSTIN DOCUMENT_NO.
    SELECT SINGLE * FROM ZGSP_CUST_DATA INTO LS_CUST_DATA WHERE FILE_TYPE = '03'.


    SELECT VBELN XBLNR FKDAT FROM VBRK
      INTO TABLE LT_VBRK
      FOR ALL ENTRIES IN GT_FINAL_OUTWARD
      WHERE VBELN = GT_FINAL_OUTWARD-DOCUMENT_NO+(10).


*********************************************************************

    TYPES: BEGIN OF TY_FINAL_OUTWARD1.
    TYPES: LOCATION_GSTIN1 TYPE ZDM_DET_LOCATION_GSTIN.
        INCLUDE STRUCTURE ZDM_GLOBAL.
    TYPES: END OF TY_FINAL_OUTWARD1.

    DATA: GT_FINAL_OUTWARD1 TYPE TABLE OF TY_FINAL_OUTWARD1,
          GS_FINAL_OUTWARD1 TYPE TY_FINAL_OUTWARD1.

    LOOP AT GT_FINAL_OUTWARD INTO LS_OUT_INVOICE..
      MOVE-CORRESPONDING LS_OUT_INVOICE TO GS_FINAL_OUTWARD1.
      GS_FINAL_OUTWARD1-LOCATION_GSTIN1 = LS_OUT_INVOICE-LOCATION_GSTIN.
      APPEND GS_FINAL_OUTWARD1 TO GT_FINAL_OUTWARD1.
      CLEAR GS_FINAL_OUTWARD1.
    ENDLOOP.

    DATA: LV_LOCATION TYPE KNA1-STCD3.

    SORT GT_FINAL_OUTWARD1 BY LOCATION_GSTIN1.
    LOOP AT GT_FINAL_OUTWARD1 INTO GS_FINAL_OUTWARD1.
      AT NEW LOCATION_GSTIN1.

        LS_FINAL-BUSINESS_PLACE         = 'Business Place'.
        LS_FINAL-LOCATION_GSTIN         = 'Location GSTIN'.
        LS_FINAL-CUSTOMER_NAME          =  'Customer Name'.
        LS_FINAL-CUSTOMER_GSTIN          =  'Customer GSTIN'.
        LS_FINAL-PLACE_OF_SUPPLY        = 'Place Of Supply'.
        LS_FINAL-GSTIN_ECOMMERCE        = 'GSTIN Of Ecommerce Operator'.
        LS_FINAL-LEGAL_NAME             =  'Legal Name Of Ecommerce Operator'.
        LS_FINAL-DOCUMENT_TYPE          = 'Document Type'.
        LS_FINAL-SALES_INVOICE          = 'Sales Invoice Number/Note Number'.
        LS_FINAL-SALES_INVOICE_DATE     = 'Sales Invoice Date'.
        LS_FINAL-INVOICE_TYPE           =  'Invoice Type'.
        LS_FINAL-SHIPPING_BILL_NUMBER   =  'Shipping Bill Number'.
        LS_FINAL-SHIPPING_BILL_DATE     = 'Shipping Bill Date'.
        LS_FINAL-PORT_CODE              = 'Port Code'.
        LS_FINAL-RCM_APPLICABILITY      = 'RCM Applicability'.
        LS_FINAL-CGST_RATE              = 'CGST Rate'.
        LS_FINAL-SGST_RATE              = 'SGST Rate'.
        LS_FINAL-IGST_RATE              = 'IGST Rate'.
        LS_FINAL-CONSOLIDATED_RATE      = 'Consolidated Rate/Rate (in %)'.
        LS_FINAL-CESS_RATE              = 'CESS Rate'.
        LS_FINAL-TAXABLE_VALUE          = 'Taxable Value'.
        LS_FINAL-DISCOUNTS              = 'Discounts in %'.
        LS_FINAL-DISCOUNTS_AMOUNT       = 'Discounts in Amount'.
        LS_FINAL-GROSS_TAXABLE_VALUE    = 'Gross Taxable Value'.
        LS_FINAL-TAX_CODE               = 'Tax Code'.
        LS_FINAL-CGST_AMOUNT            = 'CGST Amount'.
        LS_FINAL-SGST_AMOUNT            = 'SGST Amount'.
        LS_FINAL-IGST_AMOUNT            = 'IGST Amount'.
        LS_FINAL-CESS_AMOUNT            = 'CESS Amount'.
        LS_FINAL-INVOICE_VALUE          =  'Invoice Value'.
        LS_FINAL-ORIGINAL_INVOICE_NO    =  'Original Invoice No'.
        LS_FINAL-ORIGINAL_INVOICE_DATE  = 'Original Invoice Date'.
        LS_FINAL-ORIGINAL_CD_NO         = 'Original Debit-Credit Note Number'.
        LS_FINAL-ORIGINAL_CD_DATE       = 'Original Debit-Credit Note Date'.
        LS_FINAL-HSN_CODE               = 'HSN Code'.
        LS_FINAL-UNIT_OF_MEASUREMENT    = 'Unit Of Measurement'.
        LS_FINAL-QUANTITY               = 'Quantity'.
        LS_FINAL-RATE                   = 'Rate per Unit'.
        LS_FINAL-CANCELLED_DOC          = 'Cancelled Doc'.
        LS_FINAL-DOCUMENT_NUMBER        = 'Document Number'.
        LS_FINAL-DOCUMENT_DATE          = 'Document Date'.
        LS_FINAL-GL_ACCOUNT            = 'G/L Account'.
        LS_FINAL-REVENUE_GL_ACCOUNT    = 'Revenue G/L Account Description'.
        LS_FINAL-CUSTOM_FIELD1          = 'Custom Field 1'.
        LS_FINAL-CUSTOM_FIELD2          = 'Custom Field 2'.
        LS_FINAL-CUSTOM_FIELD3          = 'Custom Field 3'.
        LS_FINAL-CUSTOM_FIELD4          = 'Custom Field 4'.
        LS_FINAL-CUSTOM_FIELD5          = 'Custom Field 5'.

        APPEND LS_FINAL TO LT_FINAL.
        CLEAR: LS_FINAL.
      ENDAT.
*      MOVE-CORRESPONDING GS_FINAL_OUTWARD1 TO LS_FINAL.

      CLEAR LV_LOCATION.
      LV_LOCATION = GS_FINAL_OUTWARD1-LOCATION_GSTIN.
      LS_FINAL-BUSINESS_PLACE = GS_FINAL_OUTWARD1-CUSTOM10.
      IF LS_FINAL-BUSINESS_PLACE IS  INITIAL .
        SELECT SINGLE BUPLA FROM VBRK INTO LS_FINAL-BUSINESS_PLACE WHERE XBLNR  = GS_FINAL_OUTWARD1-DOCUMENT_NO.
      ENDIF.
      LS_FINAL-LOCATION_GSTIN = GS_FINAL_OUTWARD1-LOCATION_GSTIN.
      LS_FINAL-CUSTOMER_NAME = GS_FINAL_OUTWARD1-BILL_TO_NAME.
      LS_FINAL-CUSTOMER_GSTIN = GS_FINAL_OUTWARD1-BILL_TO_GSTIN.
      LS_FINAL-PLACE_OF_SUPPLY = GS_FINAL_OUTWARD1-POS.
      LS_FINAL-GSTIN_ECOMMERCE = GS_FINAL_OUTWARD1-GSTIN_OF_ECOMM.
      LS_FINAL-LEGAL_NAME = ''.
      LS_FINAL-DOCUMENT_TYPE = GS_FINAL_OUTWARD1-DOCUMENT_TYPE.
      LS_FINAL-SALES_INVOICE_DATE = GS_FINAL_OUTWARD1-REFERENCE_DOC_DATE.
      LS_FINAL-DOCUMENT_NUMBER = GS_FINAL_OUTWARD1-DOCUMENT_NO.
      LS_FINAL-DOCUMENT_DATE = GS_FINAL_OUTWARD1-DOCUMENT_DATE.
*      LS_FINAL-SALES_INVOICE = GS_FINAL_OUTWARD1-REFERENCE_DOC_NO.
      LS_FINAL-SALES_INVOICE = GS_FINAL_OUTWARD1-CUSTOM1.
      LS_FINAL-SALES_INVOICE_DATE = GS_FINAL_OUTWARD1-DOCUMENT_DATE.


      LS_FINAL-INVOICE_TYPE = GS_FINAL_OUTWARD1-TRANSACTION_TYPE.
      LS_FINAL-SHIPPING_BILL_NUMBER = GS_FINAL_OUTWARD1-SHIP_BILL_NO.
      LS_FINAL-SHIPPING_BILL_DATE = GS_FINAL_OUTWARD1-SHIP_BILL_DATE.
      LS_FINAL-PORT_CODE = GS_FINAL_OUTWARD1-PORT_CODE.
      LS_FINAL-RCM_APPLICABILITY = GS_FINAL_OUTWARD1-REV_CHARGES.
      LS_FINAL-CGST_RATE = GS_FINAL_OUTWARD1-CGST_RATE.
      LS_FINAL-SGST_RATE = GS_FINAL_OUTWARD1-SGST_RATE.
      LS_FINAL-IGST_RATE = GS_FINAL_OUTWARD1-IGST_RATE.
      LS_FINAL-CONSOLIDATED_RATE = ''.
      LS_FINAL-CESS_RATE = GS_FINAL_OUTWARD1-CESS_RATE.
      LS_FINAL-TAXABLE_VALUE = GS_FINAL_OUTWARD1-TAX_VALUE.
      LS_FINAL-DISCOUNTS = ''.
      LS_FINAL-DISCOUNTS_AMOUNT = GS_FINAL_OUTWARD1-DISCOUNT_AMOUNT.
      LS_FINAL-GROSS_TAXABLE_VALUE = ''.
      LS_FINAL-TAX_CODE = GS_FINAL_OUTWARD1-CUSTOM9.
      LS_FINAL-CGST_AMOUNT = GS_FINAL_OUTWARD1-CGST_AMOUNT.
      LS_FINAL-SGST_AMOUNT = GS_FINAL_OUTWARD1-SGST_AMOUNT.
      LS_FINAL-IGST_AMOUNT = GS_FINAL_OUTWARD1-IGST_AMOUNT.
      LS_FINAL-CESS_AMOUNT = GS_FINAL_OUTWARD1-CESS_AMT.
      LS_FINAL-INVOICE_VALUE = GS_FINAL_OUTWARD1-DOCUMENT_VALUE.
      LS_FINAL-ORIGINAL_INVOICE_NO = GS_FINAL_OUTWARD1-ORIG_DOC_NO.
      LS_FINAL-ORIGINAL_INVOICE_DATE = GS_FINAL_OUTWARD1-ORIG_DOC_DATE.
      LS_FINAL-HSN_CODE = GS_FINAL_OUTWARD1-HSN.
      LS_FINAL-UNIT_OF_MEASUREMENT = GS_FINAL_OUTWARD1-UOM.
      LS_FINAL-QUANTITY = GS_FINAL_OUTWARD1-QUANTITY.
      LS_FINAL-RATE = ''.
      LS_FINAL-CANCELLED_DOC = 'No'.

      LS_FINAL-GL_ACCOUNT = GS_FINAL_OUTWARD1-CUSTOM8.
      LS_FINAL-REVENUE_GL_ACCOUNT = GS_FINAL_OUTWARD1-CUSTOM9.

      IF LS_FINAL-RATE IS INITIAL.
        CLEAR : LS_FINAL-RATE.
      ENDIF.
      IF GS_FINAL_OUTWARD1-CGST_RATE IS INITIAL.
        CLEAR : LS_FINAL-CGST_RATE.
      ENDIF.
      IF GS_FINAL_OUTWARD1-CGST_AMOUNT IS INITIAL.
        CLEAR : LS_FINAL-CGST_AMOUNT.
      ENDIF.
      IF GS_FINAL_OUTWARD1-SGST_RATE IS INITIAL.
        CLEAR : LS_FINAL-SGST_RATE.
      ENDIF.
      IF GS_FINAL_OUTWARD1-SGST_AMOUNT IS INITIAL.
        CLEAR : LS_FINAL-SGST_AMOUNT.
      ENDIF.
      IF GS_FINAL_OUTWARD1-IGST_RATE IS INITIAL.
        CLEAR : LS_FINAL-IGST_RATE.
      ENDIF.
      IF GS_FINAL_OUTWARD1-IGST_AMOUNT IS INITIAL.
        CLEAR : LS_FINAL-IGST_AMOUNT.
      ENDIF.

      IF GS_FINAL_OUTWARD1-POS = GS_FINAL_OUTWARD1-BILL_TO_STATE.
        IF GS_FINAL_OUTWARD1-CGST_RATE IS INITIAL AND GS_FINAL_OUTWARD1-SGST_RATE IS INITIAL AND GS_FINAL_OUTWARD1-IGST_RATE IS INITIAL.
          LS_FINAL-CGST_RATE = 0.
          LS_FINAL-CGST_AMOUNT = 0.
          LS_FINAL-SGST_RATE = 0.
          LS_FINAL-SGST_AMOUNT = 0.
        ENDIF.
      ENDIF.
      IF GS_FINAL_OUTWARD1-POS <> GS_FINAL_OUTWARD1-BILL_TO_STATE.
        IF GS_FINAL_OUTWARD1-SGST_RATE IS INITIAL AND GS_FINAL_OUTWARD1-CGST_RATE IS NOT INITIAL AND GS_FINAL_OUTWARD1-IGST_RATE IS INITIAL.
          LS_FINAL-IGST_RATE = 0.
          LS_FINAL-IGST_AMOUNT = 0.
        ENDIF.
      ENDIF.
      IF GS_FINAL_OUTWARD1-QUANTITY IS INITIAL.
        LS_FINAL-QUANTITY = 1.
      ENDIF.
      IF GS_FINAL_OUTWARD1-POS = 99 AND GS_FINAL_OUTWARD1-SUB_SUPPLY_TYPE = 'EXPWOP' .
***        ls_final-igst_rate = 0.
***        ls_final-igst_amount = 0.
***        CLEAR: ls_final-cgst_rate,ls_final-cgst_amount,ls_final-sgst_amount,ls_final-sgst_rate.
      ELSE.
        IF GS_FINAL_OUTWARD1-CGST_RATE IS INITIAL AND GS_FINAL_OUTWARD1-SGST_RATE IS INITIAL AND GS_FINAL_OUTWARD1-IGST_RATE IS INITIAL.
          IF GS_FINAL_OUTWARD1-BILL_FROM_STATE = GS_FINAL_OUTWARD1-POS.
            LS_FINAL-CGST_RATE = 0.
            LS_FINAL-SGST_RATE = 0.
            CLEAR: LS_FINAL-IGST_RATE,LS_FINAL-IGST_AMOUNT.
          ELSE.
            LS_FINAL-IGST_RATE = 0.
            CLEAR: LS_FINAL-CGST_RATE,LS_FINAL-CGST_AMOUNT,LS_FINAL-SGST_AMOUNT,LS_FINAL-SGST_RATE.
          ENDIF.
        ENDIF.
      ENDIF.

      IF GS_FINAL_OUTWARD1-DIFF_PERCENTAGE IS INITIAL.
***        CLEAR:ls_final-diff_percentage.
      ENDIF.
      IF GS_FINAL_OUTWARD1-CESS_RATE IS INITIAL.
        CLEAR:LS_FINAL-CESS_RATE.
      ENDIF.

      CALL FUNCTION 'CLOI_PUT_SIGN_IN_FRONT'
        CHANGING
          VALUE = LS_FINAL-IGST_AMOUNT.
      CALL FUNCTION 'CLOI_PUT_SIGN_IN_FRONT'
        CHANGING
          VALUE = LS_FINAL-CGST_AMOUNT.
      CALL FUNCTION 'CLOI_PUT_SIGN_IN_FRONT'
        CHANGING
          VALUE = LS_FINAL-SGST_AMOUNT.
      CALL FUNCTION 'CLOI_PUT_SIGN_IN_FRONT'
        CHANGING
          VALUE = LS_FINAL-CESS_AMOUNT.
      CALL FUNCTION 'CLOI_PUT_SIGN_IN_FRONT'
        CHANGING
          VALUE = LS_FINAL-INVOICE_VALUE.

      IF GS_FINAL_OUTWARD1-DOCUMENT_DATE IS NOT INITIAL.
        CONCATENATE GS_FINAL_OUTWARD1-DOCUMENT_DATE+6(2) '-' GS_FINAL_OUTWARD1-DOCUMENT_DATE+4(2) '-' GS_FINAL_OUTWARD1-DOCUMENT_DATE+0(4)
                    INTO LS_FINAL-DOCUMENT_DATE.
      ELSE.
        CLEAR: LS_FINAL-DOCUMENT_DATE.
      ENDIF.

*      IF GS_FINAL_OUTWARD1-REFERENCE_DOC_DATE IS NOT INITIAL.
        IF GS_FINAL_OUTWARD1-DOCUMENT_DATE IS NOT INITIAL.
          CONCATENATE GS_FINAL_OUTWARD1-DOCUMENT_DATE+6(2) '-' GS_FINAL_OUTWARD1-DOCUMENT_DATE+4(2) '-' GS_FINAL_OUTWARD1-DOCUMENT_DATE+0(4)
            INTO LS_FINAL-SALES_INVOICE_DATE.
        ELSE.
          CLEAR : LS_FINAL-SALES_INVOICE_DATE.
        ENDIF.

*      IF LS_FINAL-SALES_INVOICE_DATE IS NOT INITIAL.
*        CONCATENATE LS_FINAL-SALES_INVOICE_DATE+0(2) '-' LS_FINAL-SALES_INVOICE_DATE+3(2) '-' LS_FINAL-SALES_INVOICE_DATE+6(4)
*          INTO LS_FINAL-SALES_INVOICE_DATE.
*      ELSE.
*        CLEAR : LS_FINAL-SALES_INVOICE_DATE.
*      ENDIF.




*      IF GS_FINAL_OUTWARD1-REFERENCE_DOC_DATE IS NOT INITIAL.
*        CONCATENATE GS_FINAL_OUTWARD1-REFERENCE_DOC_DATE+6(2) '-' GS_FINAL_OUTWARD1-REFERENCE_DOC_DATE+4(2) '-' GS_FINAL_OUTWARD1-REFERENCE_DOC_DATE+0(4)
*          INTO LS_FINAL-SALES_INVOICE_DATE.
*      ELSE.
*        CLEAR : LS_FINAL-SALES_INVOICE_DATE.
*      ENDIF.


      IF GS_FINAL_OUTWARD1-SHIP_BILL_DATE IS NOT INITIAL.
*        LV_SHIP_DATE = GS_FINAL_OUTWARD1-SHIP_BILL_DATE.
        CONCATENATE GS_FINAL_OUTWARD1-SHIP_BILL_DATE+8(2) '-' GS_FINAL_OUTWARD1-SHIP_BILL_DATE+5(2) '-' GS_FINAL_OUTWARD1-SHIP_BILL_DATE+0(4)
        INTO LS_FINAL-SHIPPING_BILL_DATE.
      ELSE.
        CLEAR: LS_FINAL-SHIPPING_BILL_DATE.
      ENDIF.

      CONDENSE:GS_FINAL_OUTWARD1-ORIG_DOC_DATE.
      IF GS_FINAL_OUTWARD1-ORIG_DOC_DATE IS NOT INITIAL.
        CONCATENATE GS_FINAL_OUTWARD1-ORIG_DOC_DATE+6(2) '-' GS_FINAL_OUTWARD1-ORIG_DOC_DATE+4(2) '-' GS_FINAL_OUTWARD1-ORIG_DOC_DATE+0(4)
                    INTO LS_FINAL-ORIGINAL_INVOICE_DATE.
      ELSE.
        CLEAR : LS_FINAL-ORIGINAL_INVOICE_DATE.
      ENDIF.


      IF LS_FINAL-ORIGINAL_INVOICE_DATE = '- -' OR LS_FINAL-ORIGINAL_INVOICE_DATE = '--'.
        CLEAR: LS_FINAL-ORIGINAL_INVOICE_DATE.
      ENDIF.

      IF LS_FINAL-SHIPPING_BILL_DATE = '- -' OR LS_FINAL-SHIPPING_BILL_DATE = '--'.
        CLEAR: LS_FINAL-SHIPPING_BILL_DATE.
      ENDIF.

      IF LS_FINAL-SALES_INVOICE_DATE = '- -' OR LS_FINAL-SALES_INVOICE_DATE = '--'.
        CLEAR: LS_FINAL-SALES_INVOICE_DATE.
      ENDIF.

      IF LS_FINAL-DOCUMENT_DATE = '- -' OR LS_FINAL-DOCUMENT_DATE = '--'.
        CLEAR: LS_FINAL-DOCUMENT_DATE.
      ENDIF.

      APPEND LS_FINAL TO LT_FINAL.
      CLEAR: LS_FINAL.

      AT END OF LOCATION_GSTIN1.
        IF LT_FINAL[] IS NOT INITIAL.
*** Data convert in CSV

*          lt_final_local = CORRESPONDING #( l_tab_taxinv_b2b_final ).

          CALL FUNCTION 'GUI_DOWNLOAD'
            EXPORTING
              FILENAME                = V_FILE
              FILETYPE                = 'ASC'
              WRITE_FIELD_SEPARATOR   = 'X'
              NO_AUTH_CHECK           = 'X'
              SHOW_TRANSFER_STATUS    = ABAP_TRUE
            TABLES
              DATA_TAB                = LT_FINAL
*             fieldnames              = lt_title
            EXCEPTIONS
              FILE_WRITE_ERROR        = 1
              NO_BATCH                = 2
              GUI_REFUSE_FILETRANSFER = 3
              INVALID_TYPE            = 4
              NO_AUTHORITY            = 5
              UNKNOWN_ERROR           = 6
              HEADER_NOT_ALLOWED      = 7
              SEPARATOR_NOT_ALLOWED   = 8
              FILESIZE_NOT_ALLOWED    = 9
              HEADER_TOO_LONG         = 10
              DP_ERROR_CREATE         = 11
              DP_ERROR_SEND           = 12
              DP_ERROR_WRITE          = 13
              UNKNOWN_DP_ERROR        = 14
              ACCESS_DENIED           = 15
              DP_OUT_OF_MEMORY        = 16
              DISK_FULL               = 17
              DP_TIMEOUT              = 18
              FILE_NOT_FOUND          = 19
              DATAPROVIDER_EXCEPTION  = 20
              CONTROL_FLUSH_ERROR     = 21
              OTHERS                  = 22.
          IF SY-SUBRC = 0.
            MESSAGE 'File downloaded successfully' TYPE 'S'.
          ENDIF.

          CLEAR:LT_FINAL.

        ENDIF.
      ENDAT.
      CLEAR : GS_FINAL_OUTWARD1.
    ENDLOOP.
  ENDIF.

ENDFORM.
FORM FILE_GENERATION.
  TYPES:
    BEGIN OF TY_OUT_INVOICE,
      BUSINESS_PLACE        TYPE CHAR50,
      LOCATION_GSTIN        TYPE CHAR50,
      CUSTOMER_NAME         TYPE CHAR50,
      CUSTOMER_GSTIN        TYPE CHAR50,
      PLACE_OF_SUPPLY       TYPE CHAR50,
      GSTIN_ECOMMERCE       TYPE CHAR50,
      LEGAL_NAME            TYPE CHAR50,
      DOCUMENT_TYPE         TYPE CHAR50,
      SALES_INVOICE         TYPE CHAR50,
      SALES_INVOICE_DATE    TYPE CHAR50,
      INVOICE_TYPE          TYPE CHAR50,
      SHIPPING_BILL_NUMBER  TYPE CHAR50,
      SHIPPING_BILL_DATE    TYPE CHAR50,
      PORT_CODE             TYPE CHAR50,
      RCM_APPLICABILITY     TYPE CHAR50,
      CGST_RATE             TYPE CHAR50,
      SGST_RATE             TYPE CHAR50,
      IGST_RATE             TYPE CHAR50,
      CONSOLIDATED_RATE     TYPE CHAR50,
      CESS_RATE             TYPE CHAR50,
      TAXABLE_VALUE         TYPE CHAR50,
      DISCOUNTS             TYPE CHAR50,
      DISCOUNTS_AMOUNT      TYPE CHAR50,
      GROSS_TAXABLE_VALUE   TYPE CHAR50,
      TAX_CODE              TYPE CHAR50,
      CGST_AMOUNT           TYPE CHAR50,
      SGST_AMOUNT           TYPE CHAR50,
      IGST_AMOUNT           TYPE CHAR50,
      CESS_AMOUNT           TYPE CHAR50,
      INVOICE_VALUE         TYPE CHAR50,
      ORIGINAL_INVOICE_NO   TYPE CHAR50,
      ORIGINAL_INVOICE_DATE TYPE CHAR50,
      ORIGINAL_CD_NO        TYPE CHAR50,
      ORIGINAL_CD_DATE      TYPE CHAR50,
      HSN_CODE              TYPE CHAR50,
      UNIT_OF_MEASUREMENT   TYPE CHAR50,
      QUANTITY              TYPE CHAR50,
      RATE                  TYPE CHAR50,
      CANCELLED_DOC         TYPE CHAR50,
      DOCUMENT_NUMBER       TYPE CHAR50,
      DOCUMENT_DATE         TYPE CHAR50,
      GL_ACCOUNT            TYPE CHAR50,
      REVENUE_GL_ACCOUNT    TYPE CHAR50,
      CUSTOM_FIELD1         TYPE CHAR50,
      CUSTOM_FIELD2         TYPE CHAR50,
      CUSTOM_FIELD3         TYPE CHAR50,
      CUSTOM_FIELD4         TYPE CHAR50,
      CUSTOM_FIELD5         TYPE CHAR50,
    END OF TY_OUT_INVOICE.
***Internal Tables******************************************************
  DATA : LT_FINAL           TYPE STANDARD TABLE OF TY_OUT_INVOICE,
         LT_DATA            TYPE TABLE OF TEXT,
         LT_OUTWARD_INVOICE TYPE TRUXS_T_TEXT_DATA,
         LT_CUST_DATA       TYPE STANDARD TABLE OF ZGSP_CUST_DATA.
***Structure***********************************************************
  DATA : LS_OUT_INVOICE TYPE TY_FINAL_OUTWARD,
         LS_FINAL       TYPE TY_OUT_INVOICE,
         LS_DATE        TYPE RANGE_S_DATS,
         LS_CUST_DATA   TYPE ZGSP_CUST_DATA.


  TYPES: BEGIN OF LTY_VBRK,
           VBELN TYPE VBRK-VBELN,
           XBLNR TYPE VBRK-XBLNR,
           FKDAT TYPE VBRK-FKDAT,
         END OF  LTY_VBRK.

  DATA: LT_VBRK TYPE TABLE OF LTY_VBRK,
        LS_VBRK TYPE LTY_VBRK.

***Variables************************************************************
  DATA : LV_PWD_LEN       TYPE I, "For finding the length of the Password, This is used when scrambling the password
         LV_HANDLE        TYPE I, "Handle for Pointing to an already connected FTP connection,used for subsequent actions on the connected FTP session
         LV_PATH          TYPE STRING, "Path that points to the FTP User's Home Directory
         LV_TSL           TYPE TIMESTAMPL, "Time Stamp
         LV_TIME_STAMP    TYPE CHAR21, "Time Stamp
         LV_BUKRS         TYPE BUKRS,
         LV_ANSWER        TYPE CHAR1,
         LV_DATE          TYPE SY-DATUM,
         L_VAR_PWD_LEN    TYPE I,
         L_VAR_HANDLE     TYPE I,
         G_VAR_RESPONSE   TYPE CHAR1,
         L_TAB_DATA       TYPE TABLE OF TEXT,
         L_VAR_PATH(100)  TYPE C,
         LV_SHIP_DATE(10) TYPE C.
***Constant*************************************************************
  CONSTANTS : L_CON_KEY       TYPE I VALUE 26101957. "Hardcoded Handler Key,This is always '26101957'
************************************************************************
  IF SY-BATCH = ABAP_FALSE.
    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
        TITLEBAR              = 'Confirmation'
        TEXT_QUESTION         = 'Please confirm, do you want to generate file on FTP?'
        TEXT_BUTTON_1         = 'Yes'
        TEXT_BUTTON_2         = 'No'
        DEFAULT_BUTTON        = '1'
        DISPLAY_CANCEL_BUTTON = ''
      IMPORTING
        ANSWER                = LV_ANSWER.
    IF LV_ANSWER = 2.
*      MESSAGE e004(zgsp_utility).
      MESSAGE 'Not Allowed to download' TYPE 'E'.
    ENDIF.
  ENDIF.
  IF GT_FINAL_OUTWARD IS NOT INITIAL.
    SORT GT_FINAL_OUTWARD BY LOCATION_GSTIN DOCUMENT_NO.
    SELECT SINGLE * FROM ZGSP_CUST_DATA INTO LS_CUST_DATA WHERE FILE_TYPE = '03'.


    SELECT VBELN XBLNR FKDAT FROM VBRK
      INTO TABLE LT_VBRK
      FOR ALL ENTRIES IN GT_FINAL_OUTWARD
      WHERE VBELN = GT_FINAL_OUTWARD-DOCUMENT_NO+(10).

    L_VAR_PWD_LEN = STRLEN( LS_CUST_DATA-FTP_PASSWARD ).
    CALL FUNCTION 'HTTP_SCRAMBLE' "For Encrypting the Password
      EXPORTING
        SOURCE      = LS_CUST_DATA-FTP_PASSWARD
        SOURCELEN   = L_VAR_PWD_LEN
        KEY         = L_CON_KEY
      IMPORTING
        DESTINATION = LS_CUST_DATA-FTP_PASSWARD.

    CALL FUNCTION 'FTP_CONNECT' "For connecting to the FTP Server's user directory
      EXPORTING
        USER            = LS_CUST_DATA-FTP_USER
        PASSWORD        = LS_CUST_DATA-FTP_PASSWARD
        HOST            = LS_CUST_DATA-FTP_HOST
        RFC_DESTINATION = LS_CUST_DATA-RFC_DESTINATION
      IMPORTING
        HANDLE          = L_VAR_HANDLE
      EXCEPTIONS
        NOT_CONNECTED   = 1
        OTHERS          = 2.
    IF SY-SUBRC <> 0.
      G_VAR_RESPONSE = 1.
      RETURN.
    ELSE.
      CALL FUNCTION 'FTP_COMMAND'
        EXPORTING
          HANDLE        = L_VAR_HANDLE
          COMMAND       = LS_CUST_DATA-REQUEST_CAMMAND
        TABLES
          DATA          = L_TAB_DATA
        EXCEPTIONS
          COMMAND_ERROR = 1
          TCPIP_ERROR   = 2.
      IF SY-SUBRC <> 0.
        G_VAR_RESPONSE = 2.
        CALL FUNCTION 'FTP_DISCONNECT' "For Disconnecting the connected FTP Session
          EXPORTING
            HANDLE = L_VAR_HANDLE
          EXCEPTIONS
            OTHERS = 1.

        CALL FUNCTION 'RFC_CONNECTION_CLOSE'
          EXPORTING
            DESTINATION = LS_CUST_DATA-RFC_DESTINATION
          EXCEPTIONS
            OTHERS      = 1.
        RETURN.
      ENDIF.
    ENDIF.
*********************************************************************

    TYPES: BEGIN OF TY_FINAL_OUTWARD1.
    TYPES: LOCATION_GSTIN1 TYPE ZDM_DET_LOCATION_GSTIN.
        INCLUDE STRUCTURE ZDM_GLOBAL.
    TYPES: END OF TY_FINAL_OUTWARD1.

    DATA: GT_FINAL_OUTWARD1 TYPE TABLE OF TY_FINAL_OUTWARD1,
          GS_FINAL_OUTWARD1 TYPE TY_FINAL_OUTWARD1.

    LOOP AT GT_FINAL_OUTWARD INTO LS_OUT_INVOICE..
      MOVE-CORRESPONDING LS_OUT_INVOICE TO GS_FINAL_OUTWARD1.
      GS_FINAL_OUTWARD1-LOCATION_GSTIN1 = LS_OUT_INVOICE-LOCATION_GSTIN.
      APPEND GS_FINAL_OUTWARD1 TO GT_FINAL_OUTWARD1.
      CLEAR GS_FINAL_OUTWARD1.
    ENDLOOP.

    DATA: LV_LOCATION TYPE KNA1-STCD3.

    SORT GT_FINAL_OUTWARD1 BY LOCATION_GSTIN1.
    LOOP AT GT_FINAL_OUTWARD1 INTO GS_FINAL_OUTWARD1.
      AT NEW LOCATION_GSTIN1.
        LS_FINAL-BUSINESS_PLACE         = 'Business Place'.
        LS_FINAL-LOCATION_GSTIN         = 'Location GSTIN'.
        LS_FINAL-CUSTOMER_NAME          =  'Customer Name'.
        LS_FINAL-CUSTOMER_GSTIN          =  'Customer GSTIN'.
        LS_FINAL-PLACE_OF_SUPPLY        = 'Place Of Supply'.
        LS_FINAL-GSTIN_ECOMMERCE        = 'GSTIN Of Ecommerce Operator'.
        LS_FINAL-LEGAL_NAME             =  'Legal Name Of Ecommerce Operator'.
        LS_FINAL-DOCUMENT_TYPE          = 'Document Type'.
        LS_FINAL-SALES_INVOICE          = 'Sales Invoice Number/Note Number'.
        LS_FINAL-SALES_INVOICE_DATE     = 'Sales Invoice Date'.
        LS_FINAL-INVOICE_TYPE           =  'Invoice Type'.
        LS_FINAL-SHIPPING_BILL_NUMBER   =  'Shipping Bill Number'.
        LS_FINAL-SHIPPING_BILL_DATE     = 'Shipping Bill Date'.
        LS_FINAL-PORT_CODE              = 'Port Code'.
        LS_FINAL-RCM_APPLICABILITY      = 'RCM Applicability'.
        LS_FINAL-CGST_RATE              = 'CGST Rate'.
        LS_FINAL-SGST_RATE              = 'SGST Rate'.
        LS_FINAL-IGST_RATE              = 'IGST Rate'.
        LS_FINAL-CONSOLIDATED_RATE      = 'Consolidated Rate/Rate (in %)'.
        LS_FINAL-CESS_RATE              = 'CESS Rate'.
        LS_FINAL-TAXABLE_VALUE          = 'Taxable Value'.
        LS_FINAL-DISCOUNTS              = 'Discounts in %'.
        LS_FINAL-DISCOUNTS_AMOUNT       = 'Discounts in Amount'.
        LS_FINAL-GROSS_TAXABLE_VALUE    = 'Gross Taxable Value'.
        LS_FINAL-TAX_CODE               = 'Tax Code'.
        LS_FINAL-CGST_AMOUNT            = 'CGST Amount'.
        LS_FINAL-SGST_AMOUNT            = 'SGST Amount'.
        LS_FINAL-IGST_AMOUNT            = 'IGST Amount'.
        LS_FINAL-CESS_AMOUNT            = 'CESS Amount'.
        LS_FINAL-INVOICE_VALUE          =  'Invoice Value'.
        LS_FINAL-ORIGINAL_INVOICE_NO    =  'Original Invoice No'.
        LS_FINAL-ORIGINAL_INVOICE_DATE  = 'Original Invoice Date'.
        LS_FINAL-ORIGINAL_CD_NO         = 'Original Debit-Credit Note Number'.
        LS_FINAL-ORIGINAL_CD_DATE       = 'Original Debit-Credit Note Date'.
        LS_FINAL-HSN_CODE               = 'HSN Code'.
        LS_FINAL-UNIT_OF_MEASUREMENT    = 'Unit Of Measurement'.
        LS_FINAL-QUANTITY               = 'Quantity'.
        LS_FINAL-RATE                   = 'Rate per Unit'.
        LS_FINAL-CANCELLED_DOC          = 'Cancelled Doc'.
        LS_FINAL-DOCUMENT_NUMBER        = 'Document Number'.
        LS_FINAL-DOCUMENT_DATE          = 'Document Date'.
        LS_FINAL-GL_ACCOUNT            = 'G/L Account'.
        LS_FINAL-REVENUE_GL_ACCOUNT    = 'Revenue G/L Account Description'.
        LS_FINAL-CUSTOM_FIELD1          = 'Custom Field 1'.
        LS_FINAL-CUSTOM_FIELD2          = 'Custom Field 2'.
        LS_FINAL-CUSTOM_FIELD3          = 'Custom Field 3'.
        LS_FINAL-CUSTOM_FIELD4          = 'Custom Field 4'.
        LS_FINAL-CUSTOM_FIELD5          = 'Custom Field 5'.

        APPEND LS_FINAL TO LT_FINAL.
        CLEAR: LS_FINAL.
      ENDAT.
*      MOVE-CORRESPONDING GS_FINAL_OUTWARD1 TO LS_FINAL.


      CLEAR LV_LOCATION.
      LV_LOCATION = GS_FINAL_OUTWARD1-LOCATION_GSTIN.

      LS_FINAL-BUSINESS_PLACE = GS_FINAL_OUTWARD1-CUSTOM10.
      LS_FINAL-LOCATION_GSTIN = GS_FINAL_OUTWARD1-LOCATION_GSTIN.
      LS_FINAL-CUSTOMER_NAME = GS_FINAL_OUTWARD1-BILL_TO_NAME.
      LS_FINAL-CUSTOMER_GSTIN = GS_FINAL_OUTWARD1-BILL_TO_GSTIN.
      LS_FINAL-PLACE_OF_SUPPLY = GS_FINAL_OUTWARD1-POS.
      LS_FINAL-GSTIN_ECOMMERCE = GS_FINAL_OUTWARD1-GSTIN_OF_ECOMM.
      LS_FINAL-LEGAL_NAME = ''.
      LS_FINAL-DOCUMENT_TYPE = GS_FINAL_OUTWARD1-DOCUMENT_TYPE.
      LS_FINAL-SALES_INVOICE_DATE = GS_FINAL_OUTWARD1-REFERENCE_DOC_DATE.
      LS_FINAL-DOCUMENT_NUMBER = GS_FINAL_OUTWARD1-DOCUMENT_NO.
      LS_FINAL-DOCUMENT_DATE = GS_FINAL_OUTWARD1-DOCUMENT_DATE.
*      LS_FINAL-SALES_INVOICE = GS_FINAL_OUTWARD1-REFERENCE_DOC_NO.
      LS_FINAL-SALES_INVOICE = GS_FINAL_OUTWARD1-CUSTOM6.
      LS_FINAL-SALES_INVOICE_DATE = GS_FINAL_OUTWARD1-CUSTOM3.

*      IF LS_FINAL-SALES_INVOICE IS INITIAL.
*        READ TABLE LT_VBRK INTO LS_VBRK WITH KEY VBELN = GS_FINAL_OUTWARD1-DOCUMENT_NO.
*        IF SY-SUBRC = 0.
*          LS_FINAL-SALES_INVOICE = LS_VBRK-XBLNR.
*          LS_FINAL-SALES_INVOICE_DATE = LS_VBRK-FKDAT.
**          LS_FINAL-SALES_INVOICE_DATE = GS_FINAL_OUTWARD1-DOCUMENT_DATE.
*        ENDIF.
*      ENDIF.

      LS_FINAL-INVOICE_TYPE = GS_FINAL_OUTWARD1-TRANSACTION_TYPE.
      LS_FINAL-SHIPPING_BILL_NUMBER = GS_FINAL_OUTWARD1-SHIP_BILL_NO.
      LS_FINAL-SHIPPING_BILL_DATE = GS_FINAL_OUTWARD1-SHIP_BILL_DATE.
      LS_FINAL-PORT_CODE = GS_FINAL_OUTWARD1-PORT_CODE.
      LS_FINAL-RCM_APPLICABILITY = GS_FINAL_OUTWARD1-REV_CHARGES.
      LS_FINAL-CGST_RATE = GS_FINAL_OUTWARD1-CGST_RATE.
      LS_FINAL-SGST_RATE = GS_FINAL_OUTWARD1-SGST_RATE.
      LS_FINAL-IGST_RATE = GS_FINAL_OUTWARD1-IGST_RATE.
      LS_FINAL-CONSOLIDATED_RATE = ''.
      LS_FINAL-CESS_RATE = GS_FINAL_OUTWARD1-CESS_RATE.
      LS_FINAL-TAXABLE_VALUE = GS_FINAL_OUTWARD1-TAX_VALUE.
      LS_FINAL-DISCOUNTS = ''.
      LS_FINAL-DISCOUNTS_AMOUNT = GS_FINAL_OUTWARD1-DISCOUNT_AMOUNT.
      LS_FINAL-GROSS_TAXABLE_VALUE = ''.
      LS_FINAL-TAX_CODE = GS_FINAL_OUTWARD1-CUSTOM9.
      LS_FINAL-CGST_AMOUNT = GS_FINAL_OUTWARD1-CGST_AMOUNT.
      LS_FINAL-SGST_AMOUNT = GS_FINAL_OUTWARD1-SGST_AMOUNT.
      LS_FINAL-IGST_AMOUNT = GS_FINAL_OUTWARD1-IGST_AMOUNT.
      LS_FINAL-CESS_AMOUNT = GS_FINAL_OUTWARD1-CESS_AMT.
      LS_FINAL-INVOICE_VALUE = GS_FINAL_OUTWARD1-DOCUMENT_VALUE.
      LS_FINAL-ORIGINAL_INVOICE_NO = GS_FINAL_OUTWARD1-ORIG_DOC_NO.
      LS_FINAL-ORIGINAL_INVOICE_DATE = GS_FINAL_OUTWARD1-ORIG_DOC_DATE.
      LS_FINAL-HSN_CODE = GS_FINAL_OUTWARD1-HSN.
      LS_FINAL-UNIT_OF_MEASUREMENT = GS_FINAL_OUTWARD1-UOM.
      LS_FINAL-QUANTITY = GS_FINAL_OUTWARD1-QUANTITY.
      LS_FINAL-RATE = ''.
      LS_FINAL-CANCELLED_DOC = 'No'.

      LS_FINAL-GL_ACCOUNT = GS_FINAL_OUTWARD1-CUSTOM8.
      LS_FINAL-REVENUE_GL_ACCOUNT = GS_FINAL_OUTWARD1-CUSTOM9.

      IF LS_FINAL-RATE IS INITIAL.
        CLEAR : LS_FINAL-RATE.
      ENDIF.
      IF GS_FINAL_OUTWARD1-CGST_RATE IS INITIAL.
        CLEAR : LS_FINAL-CGST_RATE.
      ENDIF.
      IF GS_FINAL_OUTWARD1-CGST_AMOUNT IS INITIAL.
        CLEAR : LS_FINAL-CGST_AMOUNT.
      ENDIF.
      IF GS_FINAL_OUTWARD1-SGST_RATE IS INITIAL.
        CLEAR : LS_FINAL-SGST_RATE.
      ENDIF.
      IF GS_FINAL_OUTWARD1-SGST_AMOUNT IS INITIAL.
        CLEAR : LS_FINAL-SGST_AMOUNT.
      ENDIF.
      IF GS_FINAL_OUTWARD1-IGST_RATE IS INITIAL.
        CLEAR : LS_FINAL-IGST_RATE.
      ENDIF.
      IF GS_FINAL_OUTWARD1-IGST_AMOUNT IS INITIAL.
        CLEAR : LS_FINAL-IGST_AMOUNT.
      ENDIF.

      IF GS_FINAL_OUTWARD1-POS = GS_FINAL_OUTWARD1-BILL_TO_STATE.
        IF GS_FINAL_OUTWARD1-CGST_RATE IS INITIAL AND GS_FINAL_OUTWARD1-SGST_RATE IS INITIAL AND GS_FINAL_OUTWARD1-IGST_RATE IS INITIAL.
          LS_FINAL-CGST_RATE = 0.
          LS_FINAL-CGST_AMOUNT = 0.
          LS_FINAL-SGST_RATE = 0.
          LS_FINAL-SGST_AMOUNT = 0.
        ENDIF.
      ENDIF.
      IF GS_FINAL_OUTWARD1-POS <> GS_FINAL_OUTWARD1-BILL_TO_STATE.
        IF GS_FINAL_OUTWARD1-SGST_RATE IS INITIAL AND GS_FINAL_OUTWARD1-CGST_RATE IS NOT INITIAL AND GS_FINAL_OUTWARD1-IGST_RATE IS INITIAL.
          LS_FINAL-IGST_RATE = 0.
          LS_FINAL-IGST_AMOUNT = 0.
        ENDIF.
      ENDIF.
      IF GS_FINAL_OUTWARD1-QUANTITY IS INITIAL.
        LS_FINAL-QUANTITY = 1.
      ENDIF.
      IF GS_FINAL_OUTWARD1-POS = 99 AND GS_FINAL_OUTWARD1-SUB_SUPPLY_TYPE = 'EXPWOP' .
***        ls_final-igst_rate = 0.
***        ls_final-igst_amount = 0.
***        CLEAR: ls_final-cgst_rate,ls_final-cgst_amount,ls_final-sgst_amount,ls_final-sgst_rate.
      ELSE.
        IF GS_FINAL_OUTWARD1-CGST_RATE IS INITIAL AND GS_FINAL_OUTWARD1-SGST_RATE IS INITIAL AND GS_FINAL_OUTWARD1-IGST_RATE IS INITIAL.
          IF GS_FINAL_OUTWARD1-BILL_FROM_STATE = GS_FINAL_OUTWARD1-POS.
            LS_FINAL-CGST_RATE = 0.
            LS_FINAL-SGST_RATE = 0.
            CLEAR: LS_FINAL-IGST_RATE,LS_FINAL-IGST_AMOUNT.
          ELSE.
            LS_FINAL-IGST_RATE = 0.
            CLEAR: LS_FINAL-CGST_RATE,LS_FINAL-CGST_AMOUNT,LS_FINAL-SGST_AMOUNT,LS_FINAL-SGST_RATE.
          ENDIF.
        ENDIF.
      ENDIF.

      IF GS_FINAL_OUTWARD1-DIFF_PERCENTAGE IS INITIAL.
***        CLEAR:ls_final-diff_percentage.
      ENDIF.
      IF GS_FINAL_OUTWARD1-CESS_RATE IS INITIAL.
        CLEAR:LS_FINAL-CESS_RATE.
      ENDIF.

      CALL FUNCTION 'CLOI_PUT_SIGN_IN_FRONT'
        CHANGING
          VALUE = LS_FINAL-IGST_AMOUNT.
      CALL FUNCTION 'CLOI_PUT_SIGN_IN_FRONT'
        CHANGING
          VALUE = LS_FINAL-CGST_AMOUNT.
      CALL FUNCTION 'CLOI_PUT_SIGN_IN_FRONT'
        CHANGING
          VALUE = LS_FINAL-SGST_AMOUNT.
      CALL FUNCTION 'CLOI_PUT_SIGN_IN_FRONT'
        CHANGING
          VALUE = LS_FINAL-CESS_AMOUNT.
      CALL FUNCTION 'CLOI_PUT_SIGN_IN_FRONT'
        CHANGING
          VALUE = LS_FINAL-INVOICE_VALUE.

      IF GS_FINAL_OUTWARD1-DOCUMENT_DATE IS NOT INITIAL.
        CONCATENATE GS_FINAL_OUTWARD1-DOCUMENT_DATE+6(2) '-' GS_FINAL_OUTWARD1-DOCUMENT_DATE+4(2) '-' GS_FINAL_OUTWARD1-DOCUMENT_DATE+0(4)
                    INTO LS_FINAL-DOCUMENT_DATE.
      ELSE.
        CLEAR: LS_FINAL-DOCUMENT_DATE.
      ENDIF.

*      IF GS_FINAL_OUTWARD1-REFERENCE_DOC_DATE IS NOT INITIAL.
*        IF GS_FINAL_OUTWARD1-REFERENCE_DOC_DATE IS NOT INITIAL.
*          CONCATENATE GS_FINAL_OUTWARD1-REFERENCE_DOC_DATE+6(2) '-' GS_FINAL_OUTWARD1-REFERENCE_DOC_DATE+4(2) '-' GS_FINAL_OUTWARD1-REFERENCE_DOC_DATE+0(4)
*            INTO LS_FINAL-SALES_INVOICE_DATE.
*        ELSE.
*          CLEAR : LS_FINAL-SALES_INVOICE_DATE.
*        ENDIF.

      IF LS_FINAL-SALES_INVOICE_DATE IS NOT INITIAL.
        CONCATENATE LS_FINAL-SALES_INVOICE_DATE+0(2) '-' LS_FINAL-SALES_INVOICE_DATE+3(2) '-' LS_FINAL-SALES_INVOICE_DATE+6(4)
          INTO LS_FINAL-SALES_INVOICE_DATE.
      ELSE.
        CLEAR : LS_FINAL-SALES_INVOICE_DATE.
      ENDIF.




*      IF GS_FINAL_OUTWARD1-REFERENCE_DOC_DATE IS NOT INITIAL.
*        CONCATENATE GS_FINAL_OUTWARD1-REFERENCE_DOC_DATE+6(2) '-' GS_FINAL_OUTWARD1-REFERENCE_DOC_DATE+4(2) '-' GS_FINAL_OUTWARD1-REFERENCE_DOC_DATE+0(4)
*          INTO LS_FINAL-SALES_INVOICE_DATE.
*      ELSE.
*        CLEAR : LS_FINAL-SALES_INVOICE_DATE.
*      ENDIF.


      IF GS_FINAL_OUTWARD1-SHIP_BILL_DATE IS NOT INITIAL.
*        LV_SHIP_DATE = GS_FINAL_OUTWARD1-SHIP_BILL_DATE.
        CONCATENATE GS_FINAL_OUTWARD1-SHIP_BILL_DATE+8(2) '-' GS_FINAL_OUTWARD1-SHIP_BILL_DATE+5(2) '-' GS_FINAL_OUTWARD1-SHIP_BILL_DATE+0(4)
        INTO LS_FINAL-SHIPPING_BILL_DATE.
      ELSE.
        CLEAR: LS_FINAL-SHIPPING_BILL_DATE.
      ENDIF.

      CONDENSE:GS_FINAL_OUTWARD1-ORIG_DOC_DATE.
      IF GS_FINAL_OUTWARD1-ORIG_DOC_DATE IS NOT INITIAL.
        CONCATENATE GS_FINAL_OUTWARD1-ORIG_DOC_DATE+6(2) '-' GS_FINAL_OUTWARD1-ORIG_DOC_DATE+4(2) '-' GS_FINAL_OUTWARD1-ORIG_DOC_DATE+0(4)
                    INTO LS_FINAL-ORIGINAL_INVOICE_DATE.
      ELSE.
        CLEAR : LS_FINAL-ORIGINAL_INVOICE_DATE.
      ENDIF.


      IF LS_FINAL-ORIGINAL_INVOICE_DATE = '- -' OR LS_FINAL-ORIGINAL_INVOICE_DATE = '--'.
        CLEAR: LS_FINAL-ORIGINAL_INVOICE_DATE.
      ENDIF.

      IF LS_FINAL-SHIPPING_BILL_DATE = '- -' OR LS_FINAL-SHIPPING_BILL_DATE = '--'.
        CLEAR: LS_FINAL-SHIPPING_BILL_DATE.
      ENDIF.

      IF LS_FINAL-SALES_INVOICE_DATE = '- -' OR LS_FINAL-SALES_INVOICE_DATE = '--'.
        CLEAR: LS_FINAL-SALES_INVOICE_DATE.
      ENDIF.

      IF LS_FINAL-DOCUMENT_DATE = '- -' OR LS_FINAL-DOCUMENT_DATE = '--'.
        CLEAR: LS_FINAL-DOCUMENT_DATE.
      ENDIF.

      APPEND LS_FINAL TO LT_FINAL.
      CLEAR: LS_FINAL.

      AT END OF LOCATION_GSTIN1.
        IF LT_FINAL[] IS NOT INITIAL.
*** Data convert in CSV
          CALL FUNCTION 'SAP_CONVERT_TO_TEX_FORMAT'
            EXPORTING
              I_FIELD_SEPERATOR    = ','
            TABLES
              I_TAB_SAP_DATA       = LT_FINAL
            CHANGING
              I_TAB_CONVERTED_DATA = LT_OUTWARD_INVOICE
            EXCEPTIONS
              CONVERSION_FAILED    = 1
              OTHERS               = 2.
          IF SY-SUBRC = 0.
            GET TIME STAMP FIELD LV_TSL.
            LV_TIME_STAMP = LV_TSL.
***            lv_path = 'C:\Cygnet\'.
            DATA: TEST TYPE STRING."Document_5536_08_2019_090919170404.xlsx "Time Stamp
***            CONCATENATE 'Document_5536_' P_DATE-LOW+4(2)'_' P_DATE-LOW+0(4) '_'SY-DATUM+6(2) SY-DATUM+4(2) SY-DATUM+0(4) SY-UZEIT '.' LV_TIME_STAMP+14(3) '.' LS_CUST_DATA-FTP_EXTENSION INTO L_VAR_PATH.
            CONCATENATE LV_LOCATION '_' LS_CUST_DATA-FILE_TYPE_DISC '_' S_DATE-LOW+4(2)'_' S_DATE-LOW+0(4) '_'SY-DATUM+6(2) SY-DATUM+4(2) SY-DATUM+0(4) SY-UZEIT '.' LV_TIME_STAMP+14(3) '.' LS_CUST_DATA-FTP_EXTENSION INTO L_VAR_PATH.
            CONDENSE L_VAR_PATH.
            CALL FUNCTION 'FTP_R3_TO_SERVER' "For Creating a file from SAP R3 to FTP Server
              EXPORTING
                HANDLE         = L_VAR_HANDLE
                FNAME          = L_VAR_PATH
                CHARACTER_MODE = ABAP_TRUE
              TABLES
                TEXT           = LT_OUTWARD_INVOICE "Final Internal table to be written to the text file in the FTP Server's Directory
              EXCEPTIONS
                TCPIP_ERROR    = 1
                COMMAND_ERROR  = 2
                DATA_ERROR     = 3
                OTHERS         = 4.

            IF SY-SUBRC <> 0."When FTP connection Fails
              G_VAR_RESPONSE = 5.
              EXIT.
            ELSE.
              G_VAR_RESPONSE = 0.
              FREE: LT_FINAL[], GT_FINAL_OUTWARD[].
            ENDIF.
          ENDIF.
        ELSE.
          LV_VAR_RESPONSE = 3.
        ENDIF.
      ENDAT.
      CLEAR : GS_FINAL_OUTWARD1.
    ENDLOOP.
    CALL FUNCTION 'FTP_DISCONNECT' "For Disconnecting the connected FTP Session
      EXPORTING
        HANDLE = L_VAR_HANDLE
      EXCEPTIONS
        OTHERS = 1.

    CALL FUNCTION 'RFC_CONNECTION_CLOSE'
      EXPORTING
        DESTINATION = LS_CUST_DATA-RFC_DESTINATION
      EXCEPTIONS
        OTHERS      = 1.
  ELSE.
    LV_VAR_RESPONSE = 4.
  ENDIF.


ENDFORM.
