General rules for any type of the code 

- NEVER use `# ---` and other similar block comments, do not use any sort of delimiter comments in the generated output. 
- Do not add comments. 
- Never write more than two lines per documentable entry. Fields, methods for a class are individual entries, but the function arguments are a part of the function: so the class with 10 fields might contain 11 individual documentation sentences for every field, and then one for the overall class, but no more than that. 
- Do not use emojis in the answer.
- NEVER use prefix/postfix notation for variable and member names (like `m_field`, `_field` or `field_` over regular `field`). 
- Always use comparison operators going in the same direction, `<` and `<=`, never use `>` and `>=` compare operators
- If there is no acceptable answer to the request, indicate this. 
- NEVER use `child`, `children` names as a part or as a full name -- use `nested` instead. 
- Don't ask to provide import paths for the functions or types I've mentioned, I will add import paths myself. 
- When raising errors, always construct informative error message instead of generic "error happeend, but the application won't tell you what was wrong"
  - example: `if len(set(names)) != len(names):` -- if the names are mismatched, don't just raise "names are mismatched", *find which name is the duplicate and provide user with this information*
  - example: `if name not in some_dict:` -- if the element is missing, don't raise generic "key is missing", actually *tell which key is missing, so the user would not have to do the same manual work*

# qt code rules

- Use Model View Controller design in favor of the hand-rolled sorting and filtering logic. Never write the GUI that manually rebuilds the list/table widgets on every user interaction. 
- ALWAYS use fully qualified enum name rather than the alias on the qt namespace. NEVER use the "runtime-equivalent" shorter enum accessors -- they are not handled by the static analysis tools, resulting in code that is giving false positives. 
      - BAD: `QHeaderView.Stretch`
        GOOD: `QHeaderView.ResizeMode.Stretch`
      - BAD: `Qt.NoItemFlags`
        GOOD: `Qt.ItemFlag.NoItemFlags`
- NOTE: 
  - in python slot for button "on clicked" signal should have the `checked: bool` argument, 

# python code style rules

Use double quotes for strings, use type annotations. Use pathlib library for working with files.

- Do not write type definitions unless explicitly required to for the answer. 
- Provide code only for the requested functionality. 
- Use type annotations. Use `beartype` for the function and class annotations and typing. 
  - Import `beartype.typing` instead of `typing` for type names. 
  - Use `from beartype.typing import <Type1>, <Type2>` -- do not alias the beartype typing import
- Provide full implementation of the requested logic, do not skip logic with "todo" comments. 
- When printing logs from the script, use logging module instead of `print()`
- Use `loguru` for logging Format log messages using f-strings, NEVER format the log messages using `%s`
- When writing functions returning complex data (nested dictionaries, dictionaries nested in arrays, complex tuples), consider using data classes. 
- Do not implicitly ignore errors and exceptions in the code. Unless specified as an edge case to handle, do not focus on defensive coding. If the logic is broken I want to see it fail explicitly instead of silently ignore the errors.   
- use `match .. case` statements instead of repetitive ifs -- including dispatching on the value type. 
  - syntax for matching value type is `case <type>():`
- Assume python 3.12 and above
- When writing visualization scripts never use `.show()` for any of the libraries, always save the result to the file. 
- use pytest for tests
- Prefer strictly typed data over the free-form values like strings, i.e.
  - If the field can contain only a fixed set of named values, don't use string for this, define an enum -- including fields used for the discriminator in the union types
  - NEVER write functions returning free-form `dict`, NEVER write functions returning typed tuples with four different elements or more. In both cases, define a dataclass with local name based on off the function `_SetupResultType` and the specific fields. Add documentation to the object fields.
- use `plumbum` for running commands
- If the import/runtime error is caused by the name conflict (file name is already reserved or something like that), do not attempt to write degenerate hacks around imports, tell that there is a name conflict. 

# C++ code style rules

- Provide code only for the requested functionality. Use type annotations. 
- When formatting values to string, use `fmt::format`. 
- If necessary solution might use boost libraries and range-v3 library. 
- ALWAYS Use `{}` syntax for object construction and class initializer lists, use designated field initializers when field names are known.
- ALWAYS Use `int` instead of `size_t` for size-related operations, method and loop iteration.
- ALWAYS Use `struct` instead of class
- ALWAYS Use `.at()` and `.insert_or_assign()` when working with sequential and associative containers
- NEVER split method declaration and implementation
- ALWAYS use `{}` when writing if/else/while etc. NEVER use syntax without curly braces.
- Let the exceptions in the code propagate. Unless specified as an edge case to handle, do not focus on defensive coding. If the logic is broken I want to see it fail explicitly instead of silently ignore the errors or log the warnings.
- Asume C++23 and above
