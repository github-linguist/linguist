permit (principal is User, action, resource is Agent)
when { resource.administrator };

permit (
    principal is User,
    action in [
        Action::"use",
        Action::"modify",
        Action::"delete",
        Action::"manage_ownership",
        Action::"read_secrets",
        Action::"write_secrets",
        Action::"delete_secrets"
    ],
    resource is Agent
)
when {
    principal == resource.owner && resource.workspace_access
};

permit (
    principal is User,
    action == Action::"share",
    resource is Agent
)
when {
    principal == resource.owner &&
    resource.workspace_access &&
    resource.workspace_grants.contains(Action::"share_authored")
};

permit (
    principal is User,
    action == Action::"use",
    resource is Agent
)
when {
    resource.workspace_grants.contains(Action::"use_shared") &&
    !resource.share_grants.isEmpty()
};

permit (
    principal is User,
    action == Action::"share",
    resource is Agent
)
when {
    resource.workspace_grants.contains(Action::"use_shared") &&
    resource.workspace_grants.contains(Action::"share_non_authored") &&
    resource.share_grants.contains(Action::"share_non_authored")
};

permit (
    principal is User,
    action == Action::"read_secrets",
    resource is Agent
)
when {
    resource.workspace_grants.contains(Action::"use_shared") &&
    resource.workspace_grants.contains(Action::"read_shared_secret") &&
    (
        resource.share_grants.contains(Action::"read_shared_secret") ||
        resource.share_grants.contains(Action::"write_shared_secret") ||
        resource.share_grants.contains(Action::"delete_shared_secret")
    )
};

permit (
    principal is User,
    action == Action::"write_secrets",
    resource is Agent
)
when {
    resource.workspace_grants.contains(Action::"use_shared") &&
    resource.workspace_grants.contains(Action::"read_shared_secret") &&
    resource.workspace_grants.contains(Action::"write_shared_secret") &&
    (
        resource.share_grants.contains(Action::"write_shared_secret") ||
        resource.share_grants.contains(Action::"delete_shared_secret")
    )
};

permit (
    principal is User,
    action == Action::"delete_secrets",
    resource is Agent
)
when {
    resource.workspace_grants.contains(Action::"use_shared") &&
    resource.workspace_grants.contains(Action::"read_shared_secret") &&
    resource.workspace_grants.contains(Action::"write_shared_secret") &&
    resource.workspace_grants.contains(Action::"delete_shared_secret") &&
    resource.share_grants.contains(Action::"delete_shared_secret")
};
