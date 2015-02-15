import java.sql.DriverManager;
import java.sql.Connection;
import java.sql.PreparedStatement;

public class DBDemo{
   private String protocol; //set this to some connection protocol like "jdbc:sqlserver://"
   private String dbName;   //set this to the name of your database
   private String username;
   private String password;

   Connection conn = DriverManager.getConnection(protocol + dbName, username, password);
   PreparedStatement query;

   public int setUpAndExecPS(){
      query = conn.prepareStatement(
            "UPDATE players SET name = ?, score = ?, active = ? WHERE jerseyNum = ?");

      query.setString(1, "Smith, Steve");//automatically sanitizes and adds quotes
      query.setInt(2, 42);
      query.setBoolean(3, true);
      query.setInt(4, 99);
      //there are similar methods for other SQL types in PerparedStatement

      return query.executeUpdate();//returns the number of rows changed
      //PreparedStatement.executeQuery() will return a java.sql.ResultSet,
      //execute() will simply return a boolean saying whether it succeeded or not
   }
}
