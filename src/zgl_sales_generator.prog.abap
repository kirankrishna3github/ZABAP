*&---------------------------------------------------------------------*
*& Report ZDM_SALES_GENERATOR
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*Title         : ZDM_SALES_GENERATOR                                *
* Author        :                                                    *
* Company       : cygnet                                   *
* Creation Date : 24.03.2020                                           *
* Transaction   : ZDM_SALES                                          *
* Request ID    :  IHDK905695                                           *
* Ticket No.    : NA                                                   *
* Functional C. : Nikunj Goswami                                          *
* Package       : ZCYGNET                                           *
* Description   : CYGNET Utility                 *
* NOTE : ASP extractor - DM reporting requirment
*----------------------------------------------------------------------*

REPORT zdm_global_generator.

INCLUDE ZGL_GLOBAL_GENERATOR_TOP.
*INCLUDE zdm_global_generator_top.
**
INCLUDE ZGL_GLOBAL_GENERATOR_SS.
*INCLUDE zdm_global_generator_ss.
**
INCLUDE ZGL_GLOBAL_GENERATOR_MAIN.
*INCLUDE zdm_global_generator_main.
**
INCLUDE ZGL_GLOBAL_GENERATOR_FORM.
*INCLUDE zdm_global_generator_form.
