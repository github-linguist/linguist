@AbapCatalog.sqlViewName: 'ZV_MONSTERS_ASS'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'Monster CDS View with Association'
// Listing 07.30 : Coding CDS Views using Association
define view Zcds_Monsters_Association
  as select from ztmonster_header as monster
  association [0..*] to ztmonster_pets as _Pet on monster.monster_number = _Pet.owner
{
  key monster.monster_number as MonsterNumber,
  key _Pet.pet_number        as PetNumber,
      _Pet.owner             as Owner,
      monster.name           as OwnerName,
      _Pet.pet_name          as Name,
      _Pet.pet_type          as Type,
      _Pet.pet_species       as Species,
      _Pet // Make association public
}
