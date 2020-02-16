FUNCTION zbapi_test.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(I_UNAME) TYPE  ZBAPI_UNAME
*"  EXPORTING
*"     VALUE(RETURN) TYPE  BAPIRET2
*"     VALUE(USER_DETIALS) TYPE  ZBAPI_USER_DETAILS
*"----------------------------------------------------------------------

  SELECT SINGLE * FROM usr02 INTO CORRESPONDING FIELDS OF user_detials
                  WHERE bname EQ i_uname.
  IF sy-subrc NE 0.
    return-type = 'E'.
    return-message = 'User Details not found'.
  ENDIF.



ENDFUNCTION.
