# aeplugin_rebar3 plugin

This is a rebar3 plugin for building Aeternity node plugin packages.

For an example, see [aeplugin_dev_mode](https://github.com/aeternity/aeplugin_dev_mode)

The plugin ensures that dependencies that are also dependencies of the Aeternity node,
are fetched with the same version as that used by the node the plugin is built for.

By default, dependencies will be checked against the `master` branch of the Aeternity repos,
but another branch or tag can be specified, or a local repository on disk can be used.

The command `rebar3 ae_plugin` packs the plugin application and all dependencies not
already included in the Aeternity node build into a zip (`".ez"`) archive. This archive
is then copied (or symlinked) into the `plugins/` archive of the Aeternity node, where it
is automatically picked up and extracted into `plugins/lib/`, from where it is then loaded.

## Usage

To activate the plugin, add it to the `rebar.config` file of your AE node plugin application.

```erlang
{plugins, [
           {aeplugin_rebar3, {git, "https://github.com/aeternity/aeplugin_rebar3", {branch,"master"}}}
          ]}.
```

### Specifying the Aeternity root

By default, the `master` branch of the [Aeternity repos](https://github.com/aeternity/aeternity) is used.
Specifically, the `rebar.lock` file is fetched, and dependency targets and versions are extracted.

A different root can be specified in one of two ways, listed in order of priority:

1. Create a symbolic link to an Aeternity source package on-disk. Name the link `aeternity`.
2. Specify the target as `aeternity_root` in your `rebar.config`.
   The value can either be `{local, AeternityDir}` or `{git, URI, BranchOrTag}`

```erlang
{aeternity_root, {git, "https://github.com/aeternity/aeternity", "master"}}.
```

### Name Aeternity dependencies

Dependencies that are also dependencies of the Aeternity node, are listed as `{App, {ae}}`.

Example:
```erlang
{deps, [
        {lager, {ae}},
        {cowboy, {ae}},
        {aeserialization, {ae}},
        {jsx, {ae}},
        ...
       ]}.
```

### Build the archive file

`rebar3 ae_plugin`

The `ae_plugin` command depends on `compile`, so the code will be automatically compiled first.

The archive file will be placed in the profile build directory (e.g. `_build/default`) of your
plugin application, and named `PluginName.ez`. Example:

```
aeplugin_dev_mode.ez
```
