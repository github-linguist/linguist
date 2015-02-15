import java.io.IOException;
import java.util.Arrays;

import org.apache.directory.ldap.client.api.LdapConnection;
import org.apache.directory.ldap.client.api.LdapNetworkConnection;
import org.apache.directory.shared.ldap.model.cursor.EntryCursor;
import org.apache.directory.shared.ldap.model.entry.Entry;
import org.apache.directory.shared.ldap.model.exception.LdapException;
import org.apache.directory.shared.ldap.model.message.SearchScope;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;


public class RDirectorySearchLDAP {

  protected static final Logger log_;
  private static LdapConnection connection;
  private static final String ldapHostName;
  private static final int ldapPort;
  private static final String ldapDnStr;
  private static final String ldapCreds;

  static {

    log_ = LoggerFactory.getLogger(RDirectorySearchLDAP.class);

    ldapHostName = "localhost";
    ldapPort = 11389;
    ldapDnStr="uid=admin,ou=system";
    ldapCreds="********";
  }

  public static void main(String[] args) {

    boolean connected = false;

    try {
      connected = setUp();
      if (connected) {
        search("*mil*");
      }
    }
    finally {
      if (connected) {
        tearDown();
      }
    }

    return;
  }

  private static boolean search(String uid) {

    boolean state;
    EntryCursor cursor;
    Entry ev;
    String baseDn;
    String filter;
    SearchScope scope;
    String attributes[];
    int ksearch = 0;

    state = true;

    baseDn = "ou=users,o=mojo";
    filter = "(&(objectClass=person)(&(uid=" + uid + ")))";
    scope = SearchScope.SUBTREE;
    attributes = new java.lang.String[] { "dn", "cn", "sn", "uid" };

    try {
      if (log_.isTraceEnabled()) { log_.trace("LDAP search"); }
      if (log_.isInfoEnabled()) {
        log_.info("Begin search");
        log_.info("  search base distinguished name: " + baseDn);
        log_.info("  search filter: " + filter);
        log_.info("  search attributes: " + (Arrays.asList(attributes).toString()));
      }
      cursor = connection.search(baseDn, filter, scope, attributes);
      while (cursor.next()) {
        ksearch++;
        ev = cursor.get();
        if (log_.isInfoEnabled()) { log_.info("Search cursor entry count: " + ksearch); }
        if (log_.isInfoEnabled()) { log_.info(ev.toString()); }
      }
    }
    catch (LdapException lex) {
      state = false;
      log_.error("LDAP Error in cursor loop: Iteration " + ksearch, lex);
    }
    catch (Exception ex) {
      state = false;
      log_.error("I/O Error in cursor loop: Iteration " + ksearch, ex);
    }

    return state;
  }

  private static boolean search() {

    return search("*");
  }

  private static boolean setUp() {

    boolean state = false;

    try {
      if (log_.isInfoEnabled()) { log_.info("LDAP Connection to " + ldapHostName + " on port " + ldapPort); }
      connection = new LdapNetworkConnection(ldapHostName, ldapPort);

      if (log_.isTraceEnabled()) { log_.trace("LDAP bind"); }
      connection.bind(ldapDnStr, ldapCreds);

      state = true;
    }
    catch (LdapException lex) {
      state = false;
      log_.error("LDAP Error", lex);
    }
    catch (IOException iox) {
      state = false;
      log_.error("I/O Error", iox);
    }

    return state;
  }

  private static boolean tearDown() {

    boolean state = false;

    try {
      if (log_.isTraceEnabled()) { log_.trace("LDAP unbind"); }
      connection.unBind();
      state = true;
    }
    catch (LdapException lex) {
      state = false;
      log_.error("LDAP Error", lex);
    }
    finally {
      try {
        connection.close();
      }
      catch (IOException iox) {
        state = false;
        log_.error("I/O Error on connection.close()", iox);
      }
    }

    return state;
  }
}
