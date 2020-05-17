*&---------------------------------------------------------------------*
*& Report ZDM_PURCHASE_GENERATOR
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ZGL_PURCHASE_GENERATOR.
*&---------------------------------------------------------------------*
*& Report  ZGSP_INWARD_GENERATOR
*&
*&---------------------------------------------------------------------*

*** Global Data Declaration
INCLUDE ZGL_INWARD_GENERATOR_TOP.
*INCLUDE zdm_inward_generator_top.

*** Selection Screen
INCLUDE ZGL_INWARD_GENERATOR_SS.
*INCLUDE zdm_inward_generator_ss.

*** Data Processing
INCLUDE ZGL_INWARD_GENERATOR_MAIN.
*INCLUDE zdm_inward_generator_main.

*** All the routines
INCLUDE ZGL_INWARD_GENERATOR_FORM.
*INCLUDE zdm_inward_generator_form.
**.... End of insertion of code by Mayur M on 21.01.2020 #++LRDK964118:LRDK964119 ....**
