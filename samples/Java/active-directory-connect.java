import java.io.IOException;

import org.apache.directory.ldap.client.api.LdapConnection;
import org.apache.directory.ldap.client.api.LdapNetworkConnection;
import org.apache.directory.shared.ldap.model.cursor.EntryCursor;
import org.apache.directory.shared.ldap.model.entry.Entry;
import org.apache.directory.shared.ldap.model.exception.LdapException;
import org.apache.directory.shared.ldap.model.message.SearchScope;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class RDirectoryLDAP {

  static final Logger log_;
  private static final String ldapHostName;
  private static final int ldapPort;
  private static LdapConnection connection;

  static {
    log_ = LoggerFactory.getLogger(RDirectoryLDAP.class);
    connection = null;
    ldapHostName = "localhost";
    ldapPort = 10389;
  }

  public static void main(String[] args) {
    try {
      if (log_.isInfoEnabled()) { log_.info("LDAP Connection to " + ldapHostName + " on port " + ldapPort); }
      connection = new LdapNetworkConnection(ldapHostName, ldapPort);

      try {
        if (log_.isTraceEnabled()) { log_.trace("LDAP bind"); }
        connection.bind();

        if (log_.isTraceEnabled()) { log_.trace("LDAP unbind"); }
        connection.unBind();
      }
      catch (LdapException lex) {
        log_.error("LDAP Error", lex);
      }
      catch (IOException ex) {
        log_.error("I/O Error", ex);
      }
    }
    finally {
      if (log_.isTraceEnabled()) { log_.trace("LDAP close connection"); }
      try {
        if (connection != null) {
          connection.close();
        }
      }
      catch (IOException ex) {
        log_.error("I/O Error on connection.close()", ex);
      }
    }

    return;
  }
}
