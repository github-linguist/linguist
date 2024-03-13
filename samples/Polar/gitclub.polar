allow(actor, action, resource) if
  has_permission(actor, action, resource);

# Users can see each other.
has_permission(_: User, "read", _: User);

# A User can read their own profile.
has_permission(_: User{id: id}, "read_profile", _:User{id: id});

# Any logged-in user can create a new org.
has_permission(_: User, "create", _: Org);

actor User {}

resource Org {
  roles = ["owner", "member"];
  permissions = [
    "read",
    "create_repos",
    "list_repos",
    "create_role_assignments",
    "list_role_assignments",
    "update_role_assignments",
    "delete_role_assignments",
  ];

  "read" if "member";
  "list_repos" if "member";
  "list_role_assignments" if "member";

  "create_repos" if "owner";
  "create_role_assignments" if "owner";
  "update_role_assignments" if "owner";
  "delete_role_assignments" if "owner";

  "member" if "owner";
}

has_role(user: User, name: String, org: Org) if
    role in user.org_roles and
    role matches { name: name, org: org };

resource Repo {
  roles = ["admin", "maintainer", "reader"];
  permissions = [
    "read",
    "create_issues",
    "list_issues",
    "create_role_assignments",
    "list_role_assignments",
    "update_role_assignments",
    "delete_role_assignments",
  ];
  relations = { parent: Org };

  "create_role_assignments" if "admin";
  "list_role_assignments" if "admin";
  "update_role_assignments" if "admin";
  "delete_role_assignments" if "admin";


  "read" if "reader";
  "list_issues" if "reader";
  "create_issues" if "reader";

  "admin" if "owner" on "parent";
  "reader" if "member" on "parent";

  "maintainer" if "admin";
  "reader" if "maintainer";
}

has_role(user: User, name: String, repo: Repo) if
    role in user.repo_roles and
    role matches { name: name, repo: repo };

has_relation(org: Org, "parent", repo: Repo) if repo.org = org;

resource Issue {
  roles = ["creator"];
  permissions = ["read", "close"];
  relations = { parent: Repo };
  "read" if "reader" on "parent";
  "close" if "maintainer" on "parent";
  "close" if "creator";
}

has_relation(repo: Repo, "parent", issue: Issue) if issue.repo = repo;

has_role(user: User, "creator", issue: Issue) if
    issue.creator = user;
