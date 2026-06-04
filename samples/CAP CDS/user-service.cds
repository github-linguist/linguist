/**
 * Exposes user information
 */
@requires: 'authenticated-user'
service UserService {

  /**
   * The current user
   */
  @odata.singleton entity me {
    id     : String; // user id
    locale : String;
    tenant : String;
  }

}
