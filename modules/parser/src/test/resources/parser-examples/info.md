Files in this directory are generated tests. Each directory is a test:

- the directory name is the test name
- `input.smithyql-test` contains the test's input
- `output.json` contains the expected result of parsing the input as SmithyQL and rendering it as JSON.

To run a new test, create a new directory and an `input.smithyql-test` file inside it.
You can add the `output.json` file as well, or it'll be generated the first time you run the test.
