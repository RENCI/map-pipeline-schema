# tic-mapping-schema

## Create database

```
create database <db>;
create user <user> with password '<pass>';
grant all on database <db> to <user>;
```

## Create schema in the database

```
stack build
```

```
stack exec tic-mapping-schema-exe <user> <pass> <db> <inputFile>
```

```
stack exec tic-mapping-name-exe <inputFile> <inputFile2> <outputFile>
```
