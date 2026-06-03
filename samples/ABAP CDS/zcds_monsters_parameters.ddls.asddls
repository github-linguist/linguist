@AbapCatalog.sqlViewName: 'ZV_MONSTERS_PARS'
@AbapCatalog.compiler.compareFilter: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'Monster CDS View with Parameters'
// Listing 07.32 : Defining CDS View with Parameters
define view Zcds_Monsters_Parameters
  with parameters
    p_sanity_low  : zde_monster_sanity,
    p_sanity_high : zde_monster_sanity,
    p_color       : zde_monster_color
  as select from ztmonster_header as monster
{
  key monster.monster_number    as MonsterNumber,
      monster.name              as name,
      monster.color             as color,
      monster.sanity_percentage as sanity,
      monster.strength          as strength
}
where
      monster.color     =       :p_color
  and sanity_percentage between :p_sanity_low and :p_sanity_high
