Files in this directory are generated tests.

Top-level directories signify the syntax node the test will try to parse. For example, samples in `query` will be parsed as queries, samples in `listed` as lists etc.

Inside each directory, there are more directories, which correspond 1-1 to tests.

- the directory name is the test name
- `input.smithyql-test` contains the test's input
- `output.json` contains the expected result of parsing the input as SmithyQL and rendering it as JSON.

To run a new test, create a new directory and an `input.smithyql-test` file inside it.
You can add the `output.json` file as well, or it'll be generated the first time you run the test.

If you're adding tests for a new syntax node (or tests for a node that doesn't have a test directory), you'll need to create a new test suite for it.
Follow the example of `QueryParserTests`.
