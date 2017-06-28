PROGRAM zbootstrap_test.

INCLUDE zbootstrap.

DEFINE test_method.
  methods &1 for testing.
END-OF-DEFINITION.

CLASS test_structure DEFINITION INHERITING FROM cl_aunit_assert FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PRIVATE SECTION.

    test_method create_invalid_type.
    test_method create_valid_type.
    test_method not_a_structure.
    test_method add_field.

ENDCLASS.

CLASS test_structure IMPLEMENTATION.

  METHOD create_invalid_type.

    DATA some_structure TYPE REF TO _structure.

    TRY.

        some_structure = _structure=>create_with_type_name( 'thiscannotexist' ).

      CATCH type_not_found.
        RETURN.
      CATCH ex_structure.
    ENDTRY.

    fail( `Expecting type_not_found exception` ).

  ENDMETHOD.

  METHOD create_valid_type.

    DATA some_structure TYPE REF TO _structure.

    TRY.

        some_structure = _structure=>create_with_type_name( 'reposrc' ).

      CATCH type_not_found.
        fail( ).
      CATCH ex_structure.
    ENDTRY.

    assert_equals( act = some_structure->mr_structure_description->get_relative_name( ) exp = 'REPOSRC' ).

  ENDMETHOD.

  METHOD not_a_structure.

    DATA some_structure TYPE REF TO _structure.

    TRY.

        some_structure = _structure=>create_with_type_name( 'tabname2' ).

      CATCH type_not_found.
        fail( `test broken!` ).
      CATCH ex_structure.
        RETURN.
    ENDTRY.

    fail( ).

  ENDMETHOD.

  METHOD add_field.

    DATA some_structure TYPE REF TO _structure.

    TRY.

        some_structure = _structure=>create_with_type_name( 'reposrc' ).

        some_structure->append_field(
          iv_name = 'new_field'
          iv_type = 'string'
          ).

      CATCH ex_structure.
        fail( ).
    ENDTRY.


    DATA new_component TYPE REF TO cl_abap_datadescr.

    some_structure->mr_structure_description->get_component_type(
      EXPORTING
        p_name                 = 'new_field'
      RECEIVING
        p_descr_ref            = new_component
      EXCEPTIONS
        component_not_found    = 1
        unsupported_input_type = 2
        OTHERS                 = 3
    ).
    CASE sy-subrc.
      WHEN 1.
        fail( `component not found` ).
      WHEN OTHERS.

    ENDCASE.

* Create a new data object

    DATA lr_data TYPE REF TO data.

    some_structure->create_data( IMPORTING er_data = lr_data ).

    assert_bound( lr_data ).

* try to assign something to the new field

    FIELD-SYMBOLS <data> TYPE any.

    FIELD-SYMBOLS <field> TYPE string.

    ASSIGN lr_data->* TO <data>.

    ASSIGN ('<data>-new_field') TO <field>.

    <field> = `A string`.

  ENDMETHOD.

ENDCLASS.