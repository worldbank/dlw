# dlw repository

dlw repository

## Usage

``` r
repo_create(name, local_dir = getwd(), catalog = NULL)

repo_load(name, local_dir = getwd())
```

## Arguments

- name:

  character: name of the repo (do NOT use spaces)

- local_dir:

  directory path

- catalog:

  data.table from \[dlw_server_catalog\]

## Value

invisible data.table. It saves a dlw_repo file in \`local_dir\`

Loads dlw_repo file in \`local_dir\`
