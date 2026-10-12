# Documentation Tools

The reference manual lives in the `jyro-doc` repository, under `manual/`. The scripts here generate and check it against this repository's build, so they stay beside the code they read.

Each script finds the manual in a `jyro-doc` checkout beside this repository, or under the root named by the `JYRO_DOC` environment variable.

## Generate.fsx

Writes the standard library pages, the category tables, and the error code table from the Release assemblies and the XML documentation, and refills the catalog between the `stdlib-catalog` markers of `jyro-for-llms.md`:

```
dotnet build -c Release src/Jyro.Api/Jyro.Api.fsproj
dotnet fsi docs/tools/Generate.fsx
```

A page whose first line is `<!-- hand-written -->` is never written or deleted. Every other page in a category folder is replaced.

## CheckExamples.fsx

Runs every `jyro` block in the manual and compares it with the `json` or `text` block that follows it:

```
dotnet fsi docs/tools/CheckExamples.fsx [path-under-manual]
```

The directives a block may start with are listed at the top of the script.

## CheckCSharp.fsx

Extracts every `csharp` block into a scratch project that references the Release assemblies, and builds it:

```
dotnet fsi docs/tools/CheckCSharp.fsx <scratch-project-dir>
```
