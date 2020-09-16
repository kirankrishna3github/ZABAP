*&---------------------------------------------------------------------*
*& Report ZWRKFLO_LOG
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zwrkflo_log.

TYPE-POOLS :slis.

TABLES: swwwihead.

TYPES: BEGIN OF ts_swwwihead,
         wi_id     TYPE swwwihead-wi_id,
         wi_type   TYPE swwwihead-wi_type,      "type
         wi_rhtext TYPE swwwihead-wi_rhtext,
         wi_stat   TYPE swwwihead-wi_stat,
         wi_cd     TYPE swwwihead-wi_cd,
         wi_ct     TYPE swwwihead-wi_ct,
         wi_aed    TYPE swwwihead-wi_aed,
         wi_aagent TYPE swwwihead-wi_aagent,
         TOP_WI_ID TYPE swwwihead-top_wi_id,    "top level workflow
         top_task  TYPE swwwihead-top_task,
       END OF ts_swwwihead.

TYPES : BEGIN OF ty_final,
          wi_id      TYPE swwwihead-wi_id,
          wi_rhtext  TYPE swwwihead-wi_rhtext,
          wi_stat    TYPE swwwihead-wi_stat,
          wi_cd      TYPE swwwihead-wi_cd,
          wi_ct      TYPE swwwihead-wi_ct,
          wi_aed     TYPE swwwihead-wi_aed,
          wi_aagent  TYPE swwwihead-wi_aagent,
          top_task   TYPE swwwihead-top_task,
          status     TYPE char20,
          empno(10)  TYPE c,
          tripno(15) TYPE c,
          result(4)  TYPE c,
          ename TYPE pa0001-ename,
        END OF ty_final.
"-----------DECLRATION OF INTERNAL TABLE & WORK AREA-----------

DATA: it_swihead TYPE TABLE OF ts_swwwihead,
      wa_swihead TYPE ts_swwwihead.

DATA: it_final TYPE TABLE OF ty_final,
      wa_final TYPE ty_final.

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
  SELECT-OPTIONS :" s_cd FOR swwwihead-wi_cd,
                   s_top FOR swwwihead-top_wi_id.
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

  SELECT               "CI_NOFIELD
    wi_id
    wi_type
    wi_rhtext
    wi_stat
    wi_cd
    wi_ct
    wi_aed
    wi_aagent
    top_wi_id
    top_task
    FROM swwwihead
    CLIENT SPECIFIED
    INTO TABLE it_swihead
*    WHERE wi_cd = s_cd
    WHERE top_wi_id = s_top
    and top_task = 'WS90000009'
    and wi_type = 'W'.


  DATA: lv_wid_read     TYPE sww_wiid.

  DATA: l_it_wi_container TYPE STANDARD TABLE OF swcont,
        l_wa_wi_container TYPE swcont,
        l_wa_wi_header    TYPE swwwihead.

*Select * From SWWWIHEAD INTO TABLE lt_swwwihead
*Where date = s_date.                                      " As per selection criteria

  LOOP AT it_swihead INTO wa_swihead.

  if wa_swihead-wi_aagent CA '0123456789'.

    lv_wid_read = wa_swihead-wi_id.

    CALL FUNCTION 'SWW_WI_CONTAINER_READ'
      EXPORTING
        wi_id                    = lv_wid_read
      TABLES
        wi_container             = l_it_wi_container
      CHANGING
        wi_header                = l_wa_wi_header
      EXCEPTIONS
        container_does_not_exist = 1
        read_failed              = 2
        OTHERS                   = 3.


    IF sy-subrc = 0.

      CLEAR: l_wa_wi_container.
      READ TABLE l_it_wi_container INTO l_wa_wi_container
      WITH KEY element = 'EMPLOYEENUMBER'.
      IF sy-subrc = 0.
        CONDENSE l_wa_wi_container-value.
        emp_no = l_wa_wi_container-value.
      ENDIF.

      CLEAR: l_wa_wi_container.
      READ TABLE l_it_wi_container INTO l_wa_wi_container
      WITH KEY element = 'TRIPNUMBER'.
      IF sy-subrc = 0.
        CONDENSE l_wa_wi_container-value.
        trip_no = l_wa_wi_container-value.
      ENDIF.

      CLEAR: l_wa_wi_container.
      READ TABLE l_it_wi_container INTO l_wa_wi_container
      WITH KEY element = 'RESULT'.
      IF sy-subrc = 0.
        CONDENSE l_wa_wi_container-value.
        result = l_wa_wi_container-value.
      ENDIF.


      IF  result = '0001'.
        status = ' Travel Approved'.

      ELSEIF result = '0002'.

        status = 'Travel Rejected'.
      ENDIF.

      wa_final-empno = emp_no.

  SELECT SINGLE ename
    FROM pa0001 INTO wa_final-ename
     WHERE pernr = wa_final-empno.

      wa_final-tripno = trip_no.
      wa_final-result = result.
      wa_final-status = status.

    ENDIF.

    wa_final-wi_id     = wa_swihead-wi_id.
    wa_final-wi_rhtext = wa_swihead-wi_rhtext.
    wa_final-wi_stat   = wa_swihead-wi_stat.
    wa_final-wi_cd     = wa_swihead-wi_cd.
    wa_final-wi_ct     = wa_swihead-wi_ct.
    wa_final-wi_aed    = wa_swihead-wi_aed.
    wa_final-wi_aagent = wa_swihead-wi_aagent.
    wa_final-top_task  = wa_swihead-top_task.

    APPEND wa_final TO it_final.
    CLEAR : wa_final,wa_swihead, emp_no,trip_no,result,status.

    ELSE.
      DELETE it_swihead INDEX 1 .

      ENDIF.

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

  wa_fcat-col_pos = '1' . "column position
  wa_fcat-fieldname = 'EMPNO' . "column name
  wa_fcat-tabname = 'IT_FINAL' . "table
  wa_fcat-seltext_m = 'Employee no' . "Column label
  APPEND wa_fcat TO it_fcat . "apend to fcat
  CLEAR wa_fcat .

  wa_fcat-col_pos = '2' . "column position
  wa_fcat-fieldname = 'ENAME' . "column name
  wa_fcat-tabname = 'IT_FINAL' . "table
  wa_fcat-seltext_m = 'Emp Name' . "Column label
  APPEND wa_fcat TO it_fcat . "append to fcat
  CLEAR wa_fcat .


  wa_fcat-col_pos = '3' . "column position
  wa_fcat-fieldname = 'TRIPNO' . "column name
  wa_fcat-tabname = 'IT_FINAAL' . "table
  wa_fcat-seltext_m = 'Trip no' . "Column label
  APPEND wa_fcat TO it_fcat . "append to fcat
  CLEAR wa_fcat .


*  wa_fcat-col_pos = '3' . "column position
*  wa_fcat-fieldname = 'RESULT' . "column name
*  wa_fcat-tabname = 'IT_FINAL' . "table
*  wa_fcat-seltext_m = 'Result' . "Column label
*  APPEND wa_fcat TO it_fcat . "append to fcat
*  CLEAR wa_fcat .

  wa_fcat-col_pos = '4' . "column position
  wa_fcat-fieldname = 'STATUS' . "column name
  wa_fcat-tabname = 'IT_FINAL' . "table
  wa_fcat-seltext_m = 'Approval Status' . "Column label
  APPEND wa_fcat TO it_fcat . "append to fcat
  CLEAR wa_fcat .



  wa_fcat-col_pos = '5' . "column position
  wa_fcat-fieldname = 'WI_ID' . "column name
  wa_fcat-tabname = 'IT_FINAL' . "table
  wa_fcat-seltext_m = 'Work Item ID' . "Column label
  APPEND wa_fcat TO it_fcat . "append to fcat
  CLEAR wa_fcat .

  wa_fcat-col_pos = '6' . "column position
  wa_fcat-fieldname = 'WI_RHTEXT' . "column name
  wa_fcat-tabname = 'IT_FINAL' . "table
  wa_fcat-seltext_m = 'Task Test' . "Column label
  APPEND wa_fcat TO it_fcat . "append to fcat
  CLEAR wa_fcat .

  wa_fcat-col_pos = '7' . "column position
  wa_fcat-fieldname = 'WI_STAT' . "column name
  wa_fcat-tabname = 'IT_FINAL' . "table
  wa_fcat-seltext_m = 'Workflow Status' . "Column label
  APPEND wa_fcat TO it_fcat . "append to fcat
  CLEAR wa_fcat .

  wa_fcat-col_pos = '8' . "column position
  wa_fcat-fieldname = 'WI_CD' . "column name
  wa_fcat-tabname = 'IT_FINAL' . "table
  wa_fcat-seltext_m = 'Creation Date' . "Column label
  APPEND wa_fcat TO it_fcat . "append to fcat
  CLEAR wa_fcat .

  wa_fcat-col_pos = '9' . "column position
  wa_fcat-fieldname = 'WI_CT' . "column name
  wa_fcat-tabname = 'IT_FINAL' . "table
  wa_fcat-seltext_m = 'Creation Time' . "Column label
  APPEND wa_fcat TO it_fcat . "append to fcat
  CLEAR wa_fcat .

  wa_fcat-col_pos = '10' . "column position
  wa_fcat-fieldname = 'WI_AED' . "column name
  wa_fcat-tabname = 'IT_FINAL' . "table
  wa_fcat-seltext_m = 'Done On' . "Column label
  APPEND wa_fcat TO it_fcat . "append to fcat
  CLEAR wa_fcat .

  wa_fcat-col_pos = '11' . "column position
  wa_fcat-fieldname = 'WI_AAGENT' . "column name
  wa_fcat-tabname = 'IT_FINAL' . "table
  wa_fcat-seltext_m = 'Agent' . "Column label
  APPEND wa_fcat TO it_fcat . "append to fcat
  CLEAR wa_fcat .

  wa_fcat-col_pos = '12' . "column position
  wa_fcat-fieldname = 'TOP_TASK' . "column name
  wa_fcat-tabname = 'IT_FINAL' . "table
  wa_fcat-seltext_m = 'Top Task' . "Column label
  APPEND wa_fcat TO it_fcat . "append to fcat
  CLEAR wa_fcat .

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

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program = sy-repid
*     I_CALLBACK_USER_COMMAND = 'USER_COMMAND'
      it_fieldcat        = it_fcat "PASS FIELD CATALOG TO ALV
    TABLES
      t_outtab           = it_final. "output table


ENDFORM.
