*****           Implementation of object type ZBAPI_TEST           *****
INCLUDE <OBJECT>.
BEGIN_DATA OBJECT. " Do not change.. DATA is generated
* only private members may be inserted into structure private
DATA:
" begin of private,
"   to declare private attributes remove comments and
"   insert private attributes here ...
" end of private,
      KEY LIKE SWOTOBJID-OBJKEY.
END_DATA OBJECT. " Do not change.. DATA is generated

BEGIN_METHOD ZBAPITEST CHANGING CONTAINER.
DATA:
      IUNAME LIKE ZBAPI_UNAME,
      RETURN LIKE BAPIRET2,
      USERDETIALS LIKE ZBAPI_USER_DETAILS.
  SWC_GET_ELEMENT CONTAINER 'IUname' IUNAME.
  CALL FUNCTION 'ZBAPI_TEST'
    EXPORTING
      I_UNAME = IUNAME
    IMPORTING
      RETURN = RETURN
      USER_DETIALS = USERDETIALS
    EXCEPTIONS
      OTHERS = 01.
  CASE SY-SUBRC.
    WHEN 0.            " OK
    WHEN OTHERS.       " to be implemented
  ENDCASE.
  SWC_SET_ELEMENT CONTAINER 'Return' RETURN.
  SWC_SET_ELEMENT CONTAINER 'UserDetials' USERDETIALS.
END_METHOD.
