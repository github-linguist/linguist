actor User { }

resource Organization {
    permissions = [
        "read",
        "read_details",
        "view_members",
        "manage_members",
        "set_default_role",
        "create_repositories",
        "delete"
    ];
    roles = ["admin", "member"];

    "read_details" if "member";
    "view_members" if "member";
    "create_repositories" if "member";

    "member" if "admin";
    "manage_members" if "admin";
    "set_default_role" if "admin";
    "delete" if "admin";
}

resource Repository {
    permissions = [
        "read", "create", "update", "delete",
        "invite", "write",
        "manage_jobs", "manage_issues", "create_issues",
        "view_members", "manage_members"
    ];
    roles = ["reader", "admin", "maintainer", "editor"];
    relations = { organization: Organization };

    "reader" if "member" on "organization";
    "admin" if "admin" on "organization";
    "reader" if "editor";
    "editor" if "maintainer";
    "maintainer" if "admin";

    # reader permissions
    "read" if "reader";
    "create_issues" if "reader";

    # editor permissions
    "write" if "editor";
    "manage_jobs" if "editor";
    "manage_issues" if "editor";
    "view_members" if "maintainer";

    # admin permissions
    "manage_members" if "admin";
    "update" if "admin";
    "delete" if "admin";
    "invite" if "admin" ;
}

resource Issue {
    permissions = ["read", "comment", "close"];
    roles = ["reader", "admin", "creator"];
    relations = { repository: Repository };

    "reader" if "reader" on "repository";
    "admin" if "admin" on "repository";

    "read" if "reader";
    "comment" if "admin";
    "close" if "creator";
    "close" if "admin";
}

has_permission(_: Actor, "read", repo: Repository) if
    is_public(repo);


has_permission(actor: Actor, "delete", repo: Repository) if
    has_role(actor, "member", repo) and
    is_protected(repo, false);


# readers can only comment on open issues
has_permission(actor: Actor, "comment", issue: Issue) if
    has_permission(actor, "read", issue) and
    is_closed(issue, false);


# Misc rules:
## All organizations are public
has_permission(_: User, "read", _: Organization);
has_permission(_: User, "create", "Organization");
## Users can read all users
has_permission(_: User, "read", _: User);
## Users can only read their own profiles
has_permission(user: User, "read_profile", user: User);
has_permission(_: User, "read_profile", _: User);


# Complex rules


# Actors inherit roles from groups
has_role(user: User, role: String, resource: Resource) if
    group matches Group and
    has_group(user, group) and
    has_role(group, role, resource);

# Nested group
has_group(user: User, group: Group) if
    g matches Group and
    has_group(user, g) and
    has_group(g, group);

# A custom role is defined by the permissions it grants
has_permission(actor: Actor, action: String, org: Organization) if
    role matches Role and
    has_role(actor, role, org) and
    grants_permission(role, action);

has_role(actor: Actor, role: String, repo: Repository) if
    org matches Organization and
    has_relation(repo, "organization", org) and
    has_default_role(org, role) and
    has_role(actor, "member", org);
