@id("lock_policy_get_by_id")
permit(
  principal,
  action in [Jans::Action::"GET"],
  resource == Jans::HTTP_Request::"lock_policy_get_by_id"
)
when {
context has tokens &&
  context.tokens has jans_access_token &&
  context.tokens.jans_access_token has scope &&
  context.tokens.jans_access_token.scope.contains("https://jans.io/oauth/lock/policy.readonly")
};