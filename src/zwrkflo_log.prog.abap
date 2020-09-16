*&---------------------------------------------------------------------*
*& Report ZWRKFLO_LOG
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zwrkflo_log.

TYPE-POOLS :slis.

TABLES: ptrv_head.

TYPES: BEGIN OF ts_ptrv_head,
         pernr TYPE ptrv_head-pernr,
         reinr TYPE ptrv_head-reinr,
         dates TYPE ptrv_head-dates,
         datv1 TYPE ptrv_head-datv1,
         uhrv1 TYPE ptrv_head-uhrv1,
*         sum_reimbu TYPE ptrv_head-sum_reimbu,
         sname TYPE pa0001-sname,
         kostl TYPE pa0001-kostl,
         orgeh TYPE pa0001-orgeh,
       END OF ts_ptrv_head.

TYPES : BEGIN OF ts_hrp1000,
          otype    TYPE hrp1000-otype,
          objid    TYPE hrp1000-objid,
          mc_seark TYPE hrp1000-mc_seark,   " Deparment
        END OF ts_hrp1000.

DATA: it_ptrv TYPE TABLE OF ts_ptrv_head,
      wa_ptrv  TYPE ts_ptrv_head.

DATA: it_hrp1000 TYPE TABLE OF ts_hrp1000,
      wa_hrp1000  TYPE ts_hrp1000.

TYPES : BEGIN OF ts_final,
          pernr     TYPE ptrv_head-pernr,
          reinr     TYPE ptrv_head-reinr,
          dates     TYPE ptrv_head-dates,
          datv1     TYPE ptrv_head-datv1,
          uhrv1     TYPE ptrv_head-uhrv1,
*         sum_reimbu TYPE ptrv_head-sum_reimbu,
          sname     TYPE pa0001-sname,
          kostl     TYPE pa0001-kostl,
          orgeh     TYPE pa0001-orgeh,
          mc_seark  TYPE hrp1000-mc_seark,   " Deparment
          ktext     TYPE cskt-ktext,
        END OF ts_final.

DATA: it_final TYPE TABLE OF ts_final,
      wa_final TYPE ts_final.

DATA : it_fcat      TYPE slis_t_fieldcat_alv,
       wa_fcat      TYPE slis_fieldcat_alv,
       it_header    TYPE slis_t_listheader,
       wa_header    TYPE slis_listheader,
       count        TYPE i,
       wa_layout    TYPE slis_layout_alv,
       wa_cellcolor TYPE lvc_s_scol.


DATA: emp_no(10)  TYPE c,
      trip_no(15) TYPE c,
      result(4)   TYPE c,
      status      TYPE char20.


INITIALIZATION.

  "-----------------select option-------------------------

  SELECTION-SCREEN: BEGIN OF BLOCK b1.
  SELECT-OPTIONS : s_pernr FOR ptrv_head-pernr,
                   s_datv1 FOR ptrv_head-datv1,
                   s_reinr FOR ptrv_head-reinr.
  SELECTION-SCREEN: END OF BLOCK b1.


START-OF-SELECTION.

  PERFORM get_data.
  PERFORM fcat.
  PERFORM display.


*&---------------------------------------------------------------------*
*& Form GET_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM get_data .

  SELECT
    a~pernr
    a~reinr
    a~dates
    a~datv1
    a~uhrv1
    b~sname
    b~kostl
    b~orgeh
    FROM ptrv_head AS a
    INNER JOIN pa0001 AS b
    ON a~pernr EQ b~pernr
    INTO TABLE it_ptrv
    WHERE a~pernr = s_pernr
    AND a~datv1 = s_datv1
    AND reinr = s_reinr.

  IF it_ptrv[] IS NOT INITIAL.

    SELECT
      otype
      objid
      mc_seark
      FROM hrp1000
      INTO TABLE it_hrp1000
      FOR ALL ENTRIES IN it_ptrv
      WHERE objid = it_ptrv-orgeh
      AND otype = 'O'.

  ENDIF.



  LOOP AT it_ptrv INTO wa_ptrv.

    wa_final-pernr = wa_ptrv-pernr.
    wa_final-reinr = wa_ptrv-reinr.
    wa_final-dates = wa_ptrv-dates.
    wa_final-datv1 = wa_ptrv-datv1.
    wa_final-uhrv1 = wa_ptrv-uhrv1.
    wa_final-sname = wa_ptrv-sname.
    wa_final-kostl = wa_ptrv-kostl.
    wa_final-orgeh = wa_ptrv-orgeh.

    READ TABLE it_hrp1000 INTO wa_hrp1000 WITH KEY objid = wa_final-orgeh
                                                   otype = 'O'.
    IF sy-subrc = 0.
      wa_final-mc_seark = wa_hrp1000-mc_seark.
    ENDIF.

 SELECT SINGLE ktext INTO wa_final-ktext FROM cskt
    WHERE kostl = wa_final-kostl.


  ENDLOOP.



ENDFORM.
*&---------------------------------------------------------------------*
*& Form FCAT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM fcat .

ENDFORM.
*&---------------------------------------------------------------------*
*& Form DISPLAY
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM display .

*  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
*    EXPORTING
*      i_callback_program = sy-repid
**     I_CALLBACK_USER_COMMAND = 'USER_COMMAND'
*      it_fieldcat        = it_fcat "PASS FIELD CATALOG TO ALV
*    TABLES
*      t_outtab           = it_final. "output table
*

ENDFORM.
