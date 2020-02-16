class ZUKM_R3_ACTIVATE definition
  public
  final
  create public .

*"* public components of class ZUKM_R3_ACTIVATE
*"* do not include other source files here!!!
public section.

  interfaces IF_BADI_INTERFACE .
  interfaces IF_EX_UKM_R3_ACTIVATE .
protected section.
*"* protected components of class ZUKM_R3_ACTIVATE
*"* do not include other source files here!!!
private section.
*"* private components of class ZUKM_R3_ACTIVATE
*"* do not include other source files here!!!
ENDCLASS.



CLASS ZUKM_R3_ACTIVATE IMPLEMENTATION.


method IF_EX_UKM_R3_ACTIVATE~DCD_ACTIVE.
endmethod.


method IF_EX_UKM_R3_ACTIVATE~FI_AR_UPDATE_MODE.
endmethod.


method IF_EX_UKM_R3_ACTIVATE~GET_RFCDEST_FSCM.
endmethod.


  method IF_EX_UKM_R3_ACTIVATE~NO_SLD.
  endmethod.


method IF_EX_UKM_R3_ACTIVATE~SET_ACTIVE.
  E_ACTIVE_FLAG = 'X'.
  E_ERP2005 = 'X'.
endmethod.
ENDCLASS.
