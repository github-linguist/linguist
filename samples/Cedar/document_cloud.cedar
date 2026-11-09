permit (
  principal,
  action == Action::"CreateDocument",
  resource == Drive::"drive"
);

permit (
  principal,
  action == Action::"ViewDocument",
  resource
)
when { principal == resource.owner };

permit (
  principal,
  action == Action::"ViewDocument",
  resource
)
when { principal in resource.viewACL }
unless { resource.isPrivate };

permit (
  principal == Public::"public",
  action == Action::"ViewDocument",
  resource
)
when { resource.publicAccess == "view" || resource.publicAccess == "edit" }
unless { resource.isPrivate };

permit (
  principal,
  action == Action::"ViewDocument",
  resource
)
when { principal == resource.owner };

permit (
  principal,
  action == Action::"ModifyDocument",
  resource
)
when { principal == resource.owner };

permit (
  principal,
  action == Action::"ModifyDocument",
  resource
)
when { principal in resource.modifyACL }
unless { resource.isPrivate };

permit (
  principal == Public::"public",
  action == Action::"ViewDocument",
  resource
)
when { resource.publicAccess == "edit" }
unless { resource.isPrivate };

permit (
  principal,
  action in
    [Action::"EditIsPrivate",
     Action::"AddToShareACL",
     Action::"EditPublicAccess"],
  resource
)
when { principal == resource.owner };

permit (
  principal,
  action in [Action::"AddToShareACL", Action::"EditPublicAccess"],
  resource
)
when { principal in resource.manageACL };

permit (
  principal,
  action == Action::"CreateGroup",
  resource == Drive::"drive"
);

permit (
  principal,
  action in [Action::"ModifyGroup", Action::"DeleteGroup"],
  resource
)
when { principal == resource.owner };

forbid (
  principal,
  action in
    [Action::"ViewDocument",
     Action::"ModifyDocument",
     Action::"EditIsPrivate",
     Action::"AddToShareACL",
     Action::"EditPublicAccess",
     Action::"DeleteDocument"],
  resource
)
when
{
  principal has blocked &&
  (resource.owner.blocked.contains(principal) ||
   principal.blocked.contains(resource.owner))
};

forbid (principal, action, resource)
when { !context.is_authenticated };

forbid (principal, action, resource)
when
{
  resource has owner &&
  principal != resource.owner &&
  resource has isPrivate &&
  resource.isPrivate
};
