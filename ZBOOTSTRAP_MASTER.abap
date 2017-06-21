REPORT zbootstrap_master.

* Master program for bootstrap based reports
* Copy and rename program AND the include

INCLUDE zbootstrap.

*----------------------------------------------------------------------*
*       CLASS program_model DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS program_model DEFINITION.

  PUBLIC SECTION.
    INTERFACES _imodel.

    CLASS-METHODS get_instance RETURNING VALUE(r_instance) TYPE REF TO program_model.

  PRIVATE SECTION.

    CLASS-DATA instance TYPE REF TO program_model.

    DATA report_data TYPE TABLE OF rsctt_s_txtcontainer.

ENDCLASS.                    "program_model DEFINITION

*----------------------------------------------------------------------*
*       CLASS program_model IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS program_model IMPLEMENTATION.

  METHOD get_instance.
    IF instance IS INITIAL.
      CREATE OBJECT instance.
    ENDIF.

    r_instance = instance.

  ENDMETHOD.                    "get_instance

  METHOD _imodel~initialise.
    APPEND 'Template' TO report_data.
  ENDMETHOD.                    "_imodel~initialise

  METHOD _imodel~get_data_reference.
    GET REFERENCE OF report_data INTO data_reference.
  ENDMETHOD.                    "_imodel~GET_DATA_REFERENCE

ENDCLASS.                    "program_model IMPLEMENTATION


*&---------------------------------------------------------------------*
*&      Form  _bootstrap
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM _bootstrap.

  _config=>require_version( '0.01' ).

  _config=>set_model( program_model=>get_instance( ) ).

ENDFORM.                    "_bootstrap