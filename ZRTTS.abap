*&---------------------------------------------------------------------*
*& Report  ZRTTS
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  zrtts.

INCLUDE zbootstrap.
INCLUDE zbootstrap_test.

simple_exception type_not_found.

CLASS structure DEFINITION.

  PUBLIC SECTION.

    CLASS-METHODS: create_with_type_name
      IMPORTING type_name TYPE string
      RETURNING value(r_instance) TYPE REF TO structure
      RAISING type_not_found.

ENDCLASS.

CLASS structure IMPLEMENTATION.

  METHOD create_with_type_name.

    DATA lr_type TYPE REF TO cl_abap_typedescr.

    cl_abap_typedescr=>describe_by_name(
      EXPORTING
        p_name         = type_name    " Type name
      RECEIVING
         p_descr_ref    = lr_type    " Reference to description object
       EXCEPTIONS
         type_not_found = 1
         OTHERS         = 2
    ).
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE type_not_found.
    ENDIF.

  ENDMETHOD.

ENDCLASS.

CLASS test_structure DEFINITION INHERITING FROM cl_aunit_assert FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PRIVATE SECTION.

    METHODS test_create_invalid_type FOR TESTING.

ENDCLASS.

CLASS test_structure IMPLEMENTATION.

  METHOD test_create_invalid_type.

    DATA some_structure TYPE REF TO structure.

    TRY.

        some_structure = structure=>create_with_type_name( 'thiscannotexist' ).

      CATCH type_not_found.
        RETURN.
    ENDTRY.

    fail( `Expecting type_not_found exception` ).

  ENDMETHOD.


ENDCLASS.