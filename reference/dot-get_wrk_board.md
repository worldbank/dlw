# Get working folder for saving or reading data

Determines the directory to use for saving or reading data, depending on
whether you want to use a local directory or a temporary one.

## Usage

``` r
.get_wrk_board(local, local_dir)
```

## Arguments

- local:

  logical: whether or not to save and read data locally. default is TRUE
  if \`local_dir\` exists.

- local_dir:

  character: Local directory to save data. Default available in option
  dlw.local_dir which is set initially as "".

## Value

Folder path

## Details

\- If \`local\` is \`TRUE\`, it checks if \`local_dir\` exists. If not,
it creates it (using \`fs::dir_create()\`). It then returns this
directory path. - If \`local\` is \`FALSE\`, it tries to get a temporary
directory path from the package environment
(\`get_from_dlwenv("temp_dir")\`). If this does not exist, it creates a
new temporary directory, stores its path in the environment, and returns
it.

This function ensures that data is always saved to a valid directory,
either user-specified or temporary.
