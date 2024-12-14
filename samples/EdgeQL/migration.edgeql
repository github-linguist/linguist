CREATE MIGRATION m146naaaow4uwgbxpnjq5hyizixicxvg2ccpta24pxebzfn7xeppna
    ONTO initial
{
  CREATE EXTENSION edgeql_http VERSION '1.0';
  CREATE EXTENSION graphql VERSION '1.0';
  CREATE TYPE default::Task {
      CREATE REQUIRED PROPERTY completed -> std::bool {
          SET default := false;
      };
      CREATE REQUIRED PROPERTY text -> std::str;
  };
};
