*&---------------------------------------------------------------------*
*& Report ZHR_0105
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ZHR_0105 no standard page heading line-size 255.

TYPE-POOLS TRUXS.
DATA : mode(1) VALUE 'N'.
DATA : bdcdata  LIKE bdcdata  OCCURS 0 WITH HEADER LINE.
DATA: P_FILE TYPE RLGRAP-FILENAME,
      IT_RAW TYPE TRUXS_T_TEXT_DATA,
      SNO TYPE I, FLD(30), VAL(30).


types : begin of str_tab,
           pernr like rp50g-pernr,
           subty(4)  type c,
           begda(10)  type c,
           usrid_long(241) type c,
           usrid(30) type c,
        end of str_tab.
*&--------------------------------------------------------------------*
*                 I N T E R N A L  T A B L E S                         *
*&---------------------------------------------------------------------*

  data : it_tab type standard table of str_tab,
         wa_tab type str_tab.


SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
  PARAMETERS: P_OUTFIL LIKE RLGRAP-FILENAME OBLIGATORY.
SELECTION-SCREEN END OF BLOCK B1.


AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_OUTFIL.
  CALL FUNCTION 'KD_GET_FILENAME_ON_F4'
  CHANGING FILE_NAME = P_OUTFIL
  EXCEPTIONS MASK_TOO_LONG = 1
    OTHERS = 2.
  IF SY-SUBRC <> 0.
  ENDIF.

start-of-selection.
PERFORM UPLOAD_FILE.
PERFORM RUN_BDC.

form upload_file.
  P_FILE = P_OUTFIL.
    CALL FUNCTION 'TEXT_CONVERT_XLS_TO_SAP'
    EXPORTING
*      I_FIELD_SEPERATOR =
       I_LINE_HEADER  = 'X'
       I_TAB_RAW_DATA = IT_RAW
       I_FILENAME     = P_FILE
    TABLES
      I_TAB_CONVERTED_DATA  = IT_TAB[]
    EXCEPTIONS
      CONVERSION_FAILED = 1
      OTHERS            = 2.
    IF SY-SUBRC <> 0.
      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
      WITH  SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.
*call function 'UPLOAD'
* exporting
*   filetype                      = 'DAT'
*  tables
*    data_tab                      = it_tab
* exceptions
*   conversion_error              = 1
*   invalid_table_width           = 2
*   invalid_type                  = 3
*   no_batch                      = 4
*   unknown_error                 = 5
*   gui_refuse_filetransfer       = 6
*   others                        = 7
*          .
*if sy-subrc <> 0.
* message id sy-msgid type sy-msgty number sy-msgno
*         with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*endif.
endform.



FORM RUN_BDC.

clear wa_tab.
loop at it_tab into wa_tab.
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING   INPUT         = wa_tab-pernr
   IMPORTING  OUTPUT        = wa_tab-pernr .
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING   INPUT         = wa_tab-subty
   IMPORTING  OUTPUT        = wa_tab-subty .

perform bdc_dynpro      using 'SAPMP50A' '1000'.
perform bdc_field       using 'BDC_OKCODE'
                              '=INS'.
perform bdc_field       using 'RP50G-PERNR'
                               wa_tab-pernr.
perform bdc_field       using 'RP50G-TIMR6'
                              'X'.
perform bdc_field       using 'BDC_CURSOR'
                              'RP50G-SUBTY'.
perform bdc_field       using 'RP50G-CHOIC'
                              'Communication'.
perform bdc_field       using 'RP50G-SUBTY'
                               wa_tab-subty.

if wa_tab-subty = '0010' or wa_tab-subty = '0030'.
perform bdc_dynpro      using 'MP010500' '2000'.
perform bdc_field       using 'BDC_CURSOR'
                              'P0105-USRID_LONG'.
perform bdc_field       using 'BDC_OKCODE'
                              '/00'.
perform bdc_field       using 'P0105-BEGDA'
                               wa_tab-begda.
perform bdc_field       using 'P0105-ENDDA'
                              '31.12.9999'.
perform bdc_field       using 'P0105-USRID_LONG'
                               wa_tab-usrid_long.
else.
perform bdc_dynpro      using 'MP010500' '2000'.
perform bdc_field       using 'BDC_CURSOR'
                              'P0105-USRID_LONG'.
perform bdc_field       using 'BDC_OKCODE'
                              '/00'.
perform bdc_field       using 'P0105-BEGDA'
                               wa_tab-begda.
perform bdc_field       using 'P0105-ENDDA'
                              '31.12.9999'.
perform bdc_field       using 'P0105-USRID'
                               wa_tab-usrid.

endif.
perform bdc_field       using 'BDC_OKCODE'
                               '=UPD'.
CALL TRANSACTION 'PA30' USING bdcdata
                          MODE  'A'
                          UPDATE 'S'.
CLEAR: BDCDATA. REFRESH BDCDATA.
endloop.
ENDFORM.

*----------------------------------------------------------------------*
*        Start new screen                                              *
*----------------------------------------------------------------------*
FORM BDC_DYNPRO USING PROGRAM DYNPRO.
  CLEAR BDCDATA.
  BDCDATA-PROGRAM  = PROGRAM.
  BDCDATA-DYNPRO   = DYNPRO.
  BDCDATA-DYNBEGIN = 'X'.
  APPEND BDCDATA.
ENDFORM.

*----------------------------------------------------------------------*
*        Insert field                                                  *
*----------------------------------------------------------------------*
FORM BDC_FIELD USING FNAM FVAL.
    CLEAR BDCDATA.
    BDCDATA-FNAM = FNAM.
    BDCDATA-FVAL = FVAL.
    APPEND BDCDATA.
ENDFORM.
