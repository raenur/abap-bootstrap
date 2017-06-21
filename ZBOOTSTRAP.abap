*&---------------------------------------------------------------------*
*&  Include           BOOTSTRAP v0.01
*&---------------------------------------------------------------------*


*&---------------------------------------------------------------------*
*&       Class _version
*&---------------------------------------------------------------------*
*        Text
*----------------------------------------------------------------------*
CLASS _version DEFINITION ABSTRACT FINAL.
  PUBLIC SECTION.
    CONSTANTS _top_version TYPE string VALUE '0.01'.
    CLASS-METHODS main_include_version RETURNING value(version) TYPE string.
ENDCLASS.               "_version

DEFINE simple_exception.
  class &1 definition inheriting from cx_static_check.endclass.                    "&1 DEFINITION
END-OF-DEFINITION.

DEFINE simple_exception_sub.
  class &1 definition inheriting from &2.endclass.
END-OF-DEFINITION.

* Basic exceptions
simple_exception _bootstrap_error.
simple_exception_sub _no_bootstrap_form _bootstrap_error.
simple_exception_sub _no_model_found    _bootstrap_error.

simple_exception _controller_error.

simple_exception _config_error.
simple_exception_sub _wrong_version _config_error.
simple_exception_sub _no_version_set _config_error.

*----------------------------------------------------------------------*
*       INTERFACE _imodel
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
INTERFACE _imodel.

  METHODS initialise. "Event callback

  METHODS get_data_reference RETURNING value(data_reference) TYPE REF TO data.

ENDINTERFACE.                    "program_model IMPLEMENTATION

CLASS _controller DEFINITION DEFERRED.

*----------------------------------------------------------------------*
*       CLASS _config DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS _config DEFINITION ABSTRACT FINAL FRIENDS _controller.
  PUBLIC SECTION.
    CLASS-METHODS set_model IMPORTING model TYPE REF TO _imodel.
    CLASS-METHODS set_initial_screen IMPORTING screen TYPE sydynnr.
    CLASS-METHODS get_model
      RETURNING value(model) TYPE REF TO _imodel
      RAISING _no_model_found.
    CLASS-METHODS require_version IMPORTING version TYPE string.
    CLASS-METHODS check RAISING _config_error.
  PRIVATE SECTION.
    CLASS-DATA model_ref TYPE REF TO _imodel.
    CLASS-DATA required_version TYPE string.
    CLASS-DATA initial_screen TYPE sydynnr.
ENDCLASS.                    "_config DEFINITION


*----------------------------------------------------------------------*
*       CLASS _gui DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS _gui DEFINITION ABSTRACT FINAL.

  PUBLIC SECTION.

    CLASS-METHODS progress IMPORTING text TYPE string.

ENDCLASS.                    "_gui DEFINITION

*----------------------------------------------------------------------*
*       CLASS _list DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS _list DEFINITION ABSTRACT FINAL.
  PUBLIC SECTION.
    CLASS-METHODS write IMPORTING line TYPE string.
ENDCLASS.                    "_list DEFINITION




CLASS _version IMPLEMENTATION.
  METHOD main_include_version.
    version = '0.01'.
  ENDMETHOD.                    "main_include_version
ENDCLASS.                    "_version IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS _gui IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS _gui IMPLEMENTATION.

  METHOD progress.

    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
*       percentage = 0    " Size of Bar ( 0 <= PERCENTAGE <= 100 )
        text       = text.    " Text to be Displayed

  ENDMETHOD.                    "progress

ENDCLASS.                    "_gui IMPLEMENTATION

CLASS _structure DEFINITION ABSTRACT FINAL.

  PUBLIC SECTION.

    CLASS-METHODS repeating_to_table
      IMPORTING field_prefix_sequence TYPE string
                structure             TYPE any
      EXPORTING target_table          TYPE STANDARD TABLE.

ENDCLASS.

CLASS _structure IMPLEMENTATION.

  METHOD repeating_to_table.

    IF cl_abap_typedescr=>describe_by_data( structure )->type_kind <> cl_abap_typedescr=>typekind_struct1.
      RETURN.
    ENDIF.

    DATA field_prefixes TYPE TABLE OF string.

    SPLIT field_prefix_sequence AT ',' INTO TABLE field_prefixes.

    LOOP AT field_prefixes ASSIGNING FIELD-SYMBOL(<prefix>).

      <prefix> = |STRUCTURE-{ <prefix> }|.

    ENDLOOP.

    DATA repeat_number(2) TYPE n.
    repeat_number = 1.


    DATA line TYPE REF TO data.

    DO.

      APPEND INITIAL LINE TO target_table ASSIGNING FIELD-SYMBOL(<line>).

      DATA(inserted_line) = sy-tabix.

      LOOP AT field_prefixes ASSIGNING <prefix>.

        DATA(field_accessor) = <prefix> && repeat_number.

        ASSIGN (field_accessor) TO FIELD-SYMBOL(<field>).

        IF <field> IS NOT ASSIGNED OR <field> IS INITIAL.
          CONTINUE.
        ENDIF.

        ASSIGN COMPONENT sy-tabix OF STRUCTURE <line> TO FIELD-SYMBOL(<component>).

        <component> = <field>.

        UNASSIGN <field>.

      ENDLOOP.

      IF <line> IS INITIAL.
        DELETE target_table INDEX inserted_line.
        RETURN.
      ENDIF.

      ADD 1 TO repeat_number.

    ENDDO.

  ENDMETHOD.

ENDCLASS.

*----------------------------------------------------------------------*
*       CLASS _error_list IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS _list IMPLEMENTATION.
  METHOD write.
    DATA list_line TYPE string128.

    list_line = line.

    WRITE / list_line.

  ENDMETHOD.                    "write
ENDCLASS.                    "_error_list IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS _config IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS _config IMPLEMENTATION.
  METHOD require_version.
    required_version = version.
  ENDMETHOD.                    "require_version
  METHOD check.

    IF required_version IS INITIAL.
      RAISE EXCEPTION TYPE _no_version_set.
    ENDIF.

    IF required_version NE _version=>_top_version
    OR required_version NE _version=>main_include_version( ).
      RAISE EXCEPTION TYPE _wrong_version.
    ENDIF.

  ENDMETHOD.                    "version_check
  METHOD set_model.
    model_ref = model.
  ENDMETHOD.                    "set_model
  METHOD get_model.
    IF model_ref IS BOUND.
      model = model_ref.
    ELSE.
      RAISE EXCEPTION TYPE _no_model_found.
    ENDIF.
  ENDMETHOD.                    "get_model
  METHOD set_initial_screen.
    initial_screen = screen.
  ENDMETHOD.                    "set_initial_screen
ENDCLASS.                    "_config IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS _alv_view DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS _alv_view DEFINITION.

  PUBLIC SECTION.

    EVENTS cell_link_clicked
      EXPORTING
        value(column_name) TYPE salv_de_column
        value(cell_value) TYPE any
        value(table_row) TYPE i .
    EVENTS function_selected
      EXPORTING
        value(function) TYPE string
        value(selected_row) TYPE i .

    METHODS set_data_ref
      IMPORTING
        !data_reference TYPE REF TO data .
    METHODS display .
    METHODS get_event_table
      RETURNING
        value(event_table) TYPE REF TO cl_salv_events_table .
    METHODS refresh .
    METHODS constructor .
    METHODS set_field_hotspot
      IMPORTING
        !column_name TYPE lvc_fname .
    METHODS set_column_header
      IMPORTING
        !column_name TYPE lvc_fname
        !text TYPE lvc_value .
    METHODS get_column_table
      RETURNING
        value(r_result) TYPE salv_t_column_ref .
    METHODS set_title
      IMPORTING
        !title TYPE string .
    METHODS is_row_selected
      RETURNING
        value(r_result) TYPE abap_bool .
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA alv_grid TYPE REF TO cl_salv_table .
    DATA table_data_reference TYPE REF TO data .

    METHODS column_setup .
    METHODS layout_setup
      IMPORTING
        !data_table_type TYPE REF TO cl_abap_tabledescr .
    METHODS on_link_click
      FOR EVENT link_click OF cl_salv_events_table
      IMPORTING
        !row
        !column .
    METHODS on_added_function
      FOR EVENT added_function OF cl_salv_events
      IMPORTING
        !e_salv_function .
    METHODS get_data_table_type
      IMPORTING
        !data_ref TYPE REF TO data
      RETURNING
        value(r_result) TYPE REF TO cl_abap_tabledescr .
    METHODS get_column
      IMPORTING
        !column_name TYPE lvc_fname
      RETURNING
        value(r_column) TYPE REF TO cl_salv_column_list
      RAISING
        cx_salv_not_found .
ENDCLASS.                    "_alv_view DEFINITION



*----------------------------------------------------------------------*
*       CLASS _alv_view IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS _alv_view IMPLEMENTATION.

  METHOD column_setup.

    DATA columns TYPE REF TO cl_salv_columns.

    columns = alv_grid->get_columns( ).

    columns->set_optimize( ).

  ENDMETHOD.                    "COLUMN_SETUP

  METHOD constructor.


  ENDMETHOD.                    "CONSTRUCTOR

  METHOD display.

    alv_grid->display( ).

  ENDMETHOD.                    "DISPLAY

  METHOD get_column.

    r_column ?= alv_grid->get_columns( )->get_column( column_name ).

  ENDMETHOD.                    "get_column

  METHOD get_column_table.

    r_result = alv_grid->get_columns( )->get( ).

  ENDMETHOD.                    "get_column_table

  METHOD get_data_table_type.

    r_result ?= cl_abap_tabledescr=>describe_by_data_ref( data_ref ).

  ENDMETHOD.                    "get_data_table_type

  METHOD get_event_table.

    event_table = alv_grid->get_event( ).

  ENDMETHOD.                    "GET_EVENT_TABLE

  METHOD is_row_selected.

    DATA rows TYPE salv_t_row.

    rows = alv_grid->get_selections( )->get_selected_rows( ).

    IF rows IS NOT INITIAL.
      r_result = abap_true.
    ENDIF.

  ENDMETHOD.                    "is_row_selected

  METHOD layout_setup.

    DATA: layout TYPE REF TO cl_salv_layout,
          layout_key    TYPE salv_s_layout_key.

    layout = alv_grid->get_layout( ).

    layout_key-report = sy-repid.
    layout->set_key( layout_key ).
    layout->set_save_restriction( if_salv_c_layout=>restrict_none ).

    layout->set_default( 'X' ).

  ENDMETHOD.                    "LAYOUT_SETUP

  METHOD on_added_function.

    DATA rows TYPE salv_t_row.

    rows = alv_grid->get_selections( )->get_selected_rows( ).

    DATA row TYPE i.
    READ TABLE rows INTO row INDEX 1.

    DATA function_code TYPE string.

    function_code = e_salv_function.

    RAISE EVENT function_selected EXPORTING function = function_code
                                            selected_row = row.

  ENDMETHOD.                    "on_added_function

  METHOD on_link_click.

    DATA: table_line_data_reference TYPE REF TO data.

    FIELD-SYMBOLS: <data_table>  TYPE table,
                   <table_line> TYPE any,
                   <cell_value>  TYPE any.


    ASSIGN table_data_reference->* TO <data_table>.

    IF <data_table>  IS ASSIGNED.

      CREATE DATA table_line_data_reference LIKE LINE OF <data_table>.

      ASSIGN table_line_data_reference->* TO <table_line>.

      READ TABLE <data_table> INTO <table_line> INDEX row.

      IF sy-subrc NE 0.
        MESSAGE i017(zpp_dev).
        RETURN.
      ENDIF.

    ENDIF.

    DATA field_symbol_component TYPE string.

    field_symbol_component = |<TABLE_LINE>-{ column }|.

    ASSIGN (field_symbol_component) TO <cell_value>.

    RAISE EVENT cell_link_clicked
        EXPORTING column_name = column
                  cell_value = <cell_value>
                  table_row = row.

  ENDMETHOD.                    "on_link_click

  METHOD refresh.

    DATA: stable TYPE lvc_s_stbl VALUE 'XX'.

    alv_grid->refresh(
      EXPORTING
        s_stable     = stable
        refresh_mode = if_salv_c_refresh=>full
    ).

  ENDMETHOD.                    "REFRESH

  METHOD set_column_header.

    DATA column TYPE REF TO cl_salv_column.

    TRY.
        column = alv_grid->get_columns( )->get_column( column_name ).

        column->set_short_text( |{ text }| ).

        column->set_medium_text( |{ text }| ).

        column->set_long_text( |{ text }| ).

      CATCH cx_salv_not_found.
    ENDTRY.

  ENDMETHOD.                    "set_column_header

  METHOD set_data_ref.

    me->table_data_reference = data_reference.

    FIELD-SYMBOLS: <data_table> TYPE table.

    ASSIGN data_reference->* TO <data_table>.

    TRY.
        CALL METHOD cl_salv_table=>factory
          IMPORTING
            r_salv_table = alv_grid
          CHANGING
            t_table      = <data_table>.


        DATA table_events TYPE REF TO cl_salv_events_table.

        table_events = alv_grid->get_event( ).

        SET HANDLER on_link_click FOR table_events.

        DATA action_events TYPE REF TO cl_salv_events.

        action_events = table_events.

        SET HANDLER on_added_function FOR action_events.

        alv_grid->get_selections( )->set_selection_mode( if_salv_c_selection_mode=>multiple ).

        alv_grid->get_functions( )->set_all( ).

        column_setup( ).

        layout_setup( get_data_table_type( data_reference ) ).

      CATCH cx_salv_msg.
        " do nothing
    ENDTRY.

  ENDMETHOD.                    "SET_DATA_REF

  METHOD set_field_hotspot.

    TRY.
        get_column( column_name )->set_cell_type( if_salv_c_cell_type=>hotspot ).
      CATCH cx_salv_not_found.
    ENDTRY.

  ENDMETHOD.                    "set_column_hotspot


  METHOD set_title.
    alv_grid->get_display_settings( )->set_list_header( value = |{ title }| ).
  ENDMETHOD.                    "set_title
ENDCLASS.                    "_alv_view IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS _program_view DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS _program_view DEFINITION INHERITING FROM _alv_view
  CREATE PRIVATE.

  PUBLIC SECTION.

    CLASS-METHODS get_instance RETURNING value(instance) TYPE REF TO _program_view.

    CLASS-METHODS show_information_message IMPORTING text TYPE string.

    METHODS constructor.

  PRIVATE SECTION.

    CLASS-DATA _view TYPE REF TO _program_view.

ENDCLASS.                    "_program_view DEFINITION

*----------------------------------------------------------------------*
*       CLASS _controller DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS _controller DEFINITION CREATE PRIVATE.

  PUBLIC SECTION.

    CLASS-METHODS load_of_program.

    CLASS-METHODS bootstrap RAISING _bootstrap_error _config_error.

    CLASS-METHODS start_of_selection.

    CLASS-METHODS exit_command.

    METHODS initialization.

    METHODS set_model IMPORTING model TYPE REF TO _imodel.

    METHODS set_view  IMPORTING view TYPE REF TO _program_view.

  PRIVATE SECTION.

    CLASS-DATA _controller TYPE REF TO _controller. "singleton

    CLASS-DATA _model TYPE REF TO _imodel. "singleton

    CLASS-DATA _view  TYPE REF TO _program_view. "singleton

    METHODS _start_of_selection RAISING _controller_error.

ENDCLASS.                    "_controller DEFINITION


*----------------------------------------------------------------------*
*       CLASS _controller IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS _controller IMPLEMENTATION.

  METHOD set_view.

    _view  = view.

  ENDMETHOD.                    "set_view

  METHOD set_model.

    _model = model.

  ENDMETHOD.                    "constructor

  METHOD load_of_program.
    CREATE OBJECT _controller.

  ENDMETHOD.                    "load_of_program

  METHOD initialization.


  ENDMETHOD.                    "initialization

  METHOD start_of_selection.

    TRY.
        bootstrap( ).

        _controller->_start_of_selection( ).

      CATCH _no_model_found.
        _list=>write( |No model has been set| ).
      CATCH _no_bootstrap_form.
        _list=>write( |Missing bootstrap form in main program { sy-repid }| ).
      CATCH _bootstrap_error.
        _list=>write( |Bootstrap error| ).
      CATCH _wrong_version.
        _list=>write( |Program requires different version - this is version { _version=>_top_version }| ).
      CATCH _no_version_set.
        _list=>write( |Set required version with call to _config=>require_version in _bootstrap form| ).
      CATCH _controller_error.
        _list=>write( |Controller error| ).
      CATCH _config_error.
        _list=>write( |Configuration error| ).
    ENDTRY.

  ENDMETHOD.                    "start_of_selection

  METHOD exit_command.
    IF sy-ucomm = 'GET'. "Need this to allow standard variant handling
      RETURN.
    ELSE.
      LEAVE PROGRAM.
    ENDIF.
  ENDMETHOD.                    "exit_command

  METHOD bootstrap.

    TRY.
        PERFORM _bootstrap IN PROGRAM (sy-repid).
      CATCH cx_sy_dyn_call_illegal_form.
        RAISE EXCEPTION TYPE _no_bootstrap_form.
    ENDTRY.

    _config=>check( ).

    _controller->set_model( _config=>get_model( ) ).

    _controller->set_view( _program_view=>get_instance( ) ).

  ENDMETHOD.                    "bootstrap

  METHOD _start_of_selection.

* Call selection screen
    IF _config=>initial_screen IS NOT INITIAL.
      CALL SELECTION-SCREEN _config=>initial_screen.
    ENDIF.

    _model->initialise( ).

* Determine if there was any data in the referenced table

    DATA model_data_ref TYPE REF TO data.

    model_data_ref = _model->get_data_reference( ).

    IF model_data_ref IS NOT BOUND.
      _view->show_information_message( |No data to view| ).
      RETURN.
    ENDIF.

    FIELD-SYMBOLS <table> TYPE INDEX TABLE.

    ASSIGN model_data_ref->* TO <table>.

    IF <table> IS INITIAL.
      _view->show_information_message( |No data to view| ).
      RETURN.
    ENDIF.

    _view->set_data_ref( model_data_ref ).

    _view->display( ).

  ENDMETHOD.                    "_start_of_selection

ENDCLASS.                    "_controller IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS _program_view IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS _program_view IMPLEMENTATION.

  METHOD constructor.

    super->constructor( ).

  ENDMETHOD.                    "constructor

  METHOD get_instance.

    DATA list_data_ref TYPE REF TO data.

    CREATE OBJECT _view.

    instance = _view.

  ENDMETHOD.                    "get_instance

  METHOD show_information_message.
    MESSAGE i001(00) WITH text.
  ENDMETHOD.                    "show_information_message

ENDCLASS.                    "_program_view IMPLEMENTATION

*Event Handling

LOAD-OF-PROGRAM.
  _controller=>load_of_program( ).

INITIALIZATION.

START-OF-SELECTION.

  _controller=>start_of_selection( ).

AT SELECTION-SCREEN ON EXIT-COMMAND.

  _controller=>exit_command( ).