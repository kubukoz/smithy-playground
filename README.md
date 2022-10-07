# smithy-playground

This is an experimental, work-in-progress project to develop a query language for Smithy.

Currently available as a LSP server with a client implementation for VS Code.

## Usage

1. Make sure you have [Coursier](https://get-coursier.io/docs/cli-installation) available on the PATH as `cs`
2. Get the extension from [Marketplace](https://marketplace.visualstudio.com/items?itemName=kubukoz.smithy-playground)
3. Create a file, `smithy-build.json`. Example:

```jsonc
{
  // This comes from https://github.com/aws/aws-sdk-js-v3/blob/main/codegen/sdk-codegen/aws-models/kinesis.json
  "imports": ["/Users/kubukoz/projects/aws-sdk-js-v3/codegen/sdk-codegen/aws-models/kinesis.json"],
  // For Smithy specs exported in jars
  "mavenDependencies": [
    "com.kubukoz:service-api:0.0.1"
  ],
  // To access non-standard Maven repositories
  "mavenRepositories": [
    "https://<path-to-artifactory>"
  ]
}
```

4. Open/create a .smithyql file
5. You should see output in the "Smithy Playground" panel, and after a while syntax/error highlighting in the open .smithyql files.
