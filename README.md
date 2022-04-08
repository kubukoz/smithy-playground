# smithy-playground

This is an experimental, work-in-progress project to develop a query language for Smithy.

Currently, everything is only available as a VS Code plugin, with future ideas to extract a Language Server.

## Usage

Note: this currently doesn't work :) check back later. The reason is that the `smithy4s-codegen` binary's filesystem path is hardcoded.

1. Get the extension from [Marketplace](https://marketplace.visualstudio.com/items?itemName=kubukoz.smithy-playground) or build from source (instructions below).
2. Create a file, `smithy-build.json`. Example:

```json
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

3. Reload the window / open the project with this file again
4. Open/create a .smithyql file
5. You should see output in the "Smithy Playground" panel, and after a while syntax/error highlighting in the open .smithyql files.

## Development

1. Use nix and enter the shell, or make sure you have `yarn` and `sbt` on the PATH
2. Run `sbtn` to start an sbt server - after it loads, you can close it at any time, it'll run until you use the `shutdown` command
3. Use the attached launch configurations to run the extension: it should run `sbtn fastOptJS` and load the compiled extension.

