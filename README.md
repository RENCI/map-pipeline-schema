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
Run the following command in map-pipeline-schema directory.
```
stack exec tic-mapping-name-exe <inputFile> <outputFile>
```
where the <inputFile> is redcap data dictionary json file 
and outputFile is tables.sql file. For example, run it 
as follows:
```
stack exec map-pipeline-schema-exe ../mapping.json ../data/tables.sql
```
