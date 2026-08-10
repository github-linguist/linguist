// -- Determines what equipment slots should have hidden gear

// -- To hide gear for chosen slot just uncomment slot line
//    (by removing that doubled slash symbol from the start of the line)
public static func ShouldHideHG(area: gamedataEquipmentArea) -> Bool {
  return
    Equals(area, gamedataEquipmentArea.Head) ||
    // Equals(area, gamedataEquipmentArea.Face) ||
    // Equals(area, gamedataEquipmentArea.Feet) ||
    // Equals(area, gamedataEquipmentArea.Legs) ||
    // Equals(area, gamedataEquipmentArea.InnerChest) ||
    // Equals(area, gamedataEquipmentArea.OuterChest) ||
  false;
}


// -- Do not edit anything below

public static func ShouldHideHG(itemId: ItemID) -> Bool {
  let area: gamedataEquipmentArea = EquipmentSystem.GetEquipAreaType(itemId);
  return ShouldHideHG(area);
}

public static func ShouldDisplayHG(itemId: ItemID) -> Bool {
  return !ShouldHideHG(itemId) || IsTppHeadHG(itemId);
}

public static func IsTppHeadHG(itemId: ItemID) -> Bool {
  return Equals(ItemID.GetTDBID(itemId), t"Items.PlayerMaTppHead") || Equals(ItemID.GetTDBID(itemId), t"Items.PlayerWaTppHead");
}


// Instant unequip does not fix mirror TPP view so need delayed one

public class DelayedUnequipCallback extends DelayCallback {
  public let playerData: ref<EquipmentSystemPlayerData>;
  public func Call() -> Void {
    this.playerData.UnequipHeadSlot();
  }
}

public class DelayedReequipCallback extends DelayCallback {
  public let playerData: ref<EquipmentSystemPlayerData>;
  public func Call() -> Void {
    this.playerData.ReequipHeadSlot();
  }
}


// [EquipmentSystemPlayerData]

@addField(EquipmentSystemPlayerData)
private let m_unequipped: Bool;

@addField(EquipmentSystemPlayerData)
private let m_unequippedId: ItemID;

@addField(EquipmentSystemPlayerData)
private let m_unequippedSlot: Int32;

@addMethod(EquipmentSystemPlayerData)
public func ClearHeadSlot() -> Void {
  GameInstance.GetTransactionSystem(this.m_owner.GetGame()).RemoveItemFromSlot(this.m_owner, t"AttachmentSlots.Head");
}

@addMethod(EquipmentSystemPlayerData)
public func UnequipHeadSlot() -> Void {
  this.m_unequippedId = this.GetActiveItem(gamedataEquipmentArea.Head);
  this.m_unequippedSlot = this.GetSlotIndex(this.m_unequippedId);
  let unequipRequest: ref<UnequipRequest>;
  if ShouldHideHG(gamedataEquipmentArea.Head) {
    unequipRequest = new UnequipRequest();
    this.m_unequipped = true;
    unequipRequest.slotIndex = this.m_unequippedSlot;
    unequipRequest.areaType = gamedataEquipmentArea.Head;
    unequipRequest.owner = this.m_owner;
    EquipmentSystem.GetInstance(this.m_owner).QueueRequest(unequipRequest);
  };
}

@addMethod(EquipmentSystemPlayerData)
public func ReequipHeadSlot() -> Void {
  let equipRequest: ref<EquipRequest>;

  if this.m_unequipped {
    this.m_unequipped = false;
    equipRequest = new EquipRequest();
    equipRequest.itemID = this.m_unequippedId;
    equipRequest.slotIndex = this.m_unequippedSlot;
    equipRequest.owner = this.m_owner;
    EquipmentSystem.GetInstance(this.m_owner).QueueRequest(equipRequest);
  };
}

@addMethod(EquipmentSystemPlayerData)
private func ShouldHideAreaByTag(tag: CName) -> Bool {
  switch (tag) {
    case n"hide_H1": return ShouldHideHG(gamedataEquipmentArea.Head);
    case n"hide_F1": return ShouldHideHG(gamedataEquipmentArea.Face);
    case n"hide_S1": return ShouldHideHG(gamedataEquipmentArea.Feet);
    case n"hide_L1": return ShouldHideHG(gamedataEquipmentArea.Legs);
    case n"hide_T1": return ShouldHideHG(gamedataEquipmentArea.InnerChest);
    case n"hide_T2": return ShouldHideHG(gamedataEquipmentArea.OuterChest);
    default: return false;
  };
}

// Seems to work better comparing to OnEquipProcessVisualTags hacks
@wrapMethod(EquipmentSystemPlayerData)
private final func IsVisualTagActive(tag: CName) -> Bool {
  if this.ShouldHideAreaByTag(tag) {
    return true;
  } else {
    return wrappedMethod(tag);
  };
}

// Additional hack to force items hidden state
@wrapMethod(EquipmentSystemPlayerData)
public final func IsSlotHidden(area: gamedataEquipmentArea) -> Bool {
  return wrappedMethod(area) || ShouldHideHG(area);
}

// Hack EquipItem calls to prevent inventory preview item disappearing
@replaceMethod(EquipmentSystemPlayerData)
private final func EquipItem(itemID: ItemID, slotIndex: Int32, opt blockActiveSlotsUpdate: Bool, opt forceEquipWeapon: Bool) -> Void {
  let audioEventFoley: ref<AudioEvent>;
  let audioEventFootwear: ref<AudioEvent>;
  let currentItem: ItemID;
  let currentItemData: wref<gameItemData>;
  let cyberwareType: CName;
  let equipArea: SEquipArea;
  let equipAreaIndex: Int32;
  let i: Int32;
  let paperdollEquipData: SPaperdollEquipData;
  let placementSlot: TweakDBID;
  let transactionSystem: ref<TransactionSystem>;
  let weaponRecord: ref<WeaponItem_Record>;
  let itemData: wref<gameItemData> = RPGManager.GetItemData(this.m_owner.GetGame(), this.m_owner, itemID);
  if !this.IsEquippable(itemData) {
    return;
  };
  if Equals(RPGManager.GetItemRecord(itemID).ItemType().Type(), gamedataItemType.Cyb_StrongArms) {
    this.HandleStrongArmsEquip(itemID);
  };
  equipAreaIndex = this.GetEquipAreaIndex(EquipmentSystem.GetEquipAreaType(itemID));
  equipArea = this.m_equipment.equipAreas[equipAreaIndex];
  currentItem = this.m_equipment.equipAreas[equipAreaIndex].equipSlots[slotIndex].itemID;
  currentItemData = RPGManager.GetItemData(this.m_owner.GetGame(), this.m_owner, currentItem);
  if IsDefined(currentItemData) && currentItemData.HasTag(n"UnequipBlocked") {
    return;
  };
  if this.IsItemOfCategory(itemID, gamedataItemCategory.Weapon) && equipArea.activeIndex == slotIndex && this.CheckWeaponAgainstGameplayRestrictions(itemID) && !blockActiveSlotsUpdate {
    this.SetSlotActiveItem(EquipmentManipulationRequestSlot.Right, itemID);
    this.SetLastUsedItem(itemID);
    this.SendPSMWeaponManipulationRequest(EquipmentManipulationRequestType.Equip, EquipmentManipulationRequestSlot.Right, gameEquipAnimationType.Default);
  } else {
    if this.IsItemOfCategory(itemID, gamedataItemCategory.Weapon) && forceEquipWeapon && this.CheckWeaponAgainstGameplayRestrictions(itemID) {
      this.m_equipment.equipAreas[equipAreaIndex].equipSlots[slotIndex].itemID = itemID;
      this.SetSlotActiveItem(EquipmentManipulationRequestSlot.Right, itemID);
      this.UpdateEquipAreaActiveIndex(itemID);
      this.SetLastUsedItem(itemID);
    } else {
      this.UnequipItem(equipAreaIndex, slotIndex);
      cyberwareType = TweakDBInterface.GetCName(ItemID.GetTDBID(itemID) + t".cyberwareType", n"");
      i = 0;
      while i < ArraySize(this.m_equipment.equipAreas[equipAreaIndex].equipSlots) {
        if Equals(cyberwareType, TweakDBInterface.GetCName(ItemID.GetTDBID(this.m_equipment.equipAreas[equipAreaIndex].equipSlots[i].itemID) + t".cyberwareType", n"type")) {
          this.UnequipItem(equipAreaIndex, i);
        };
        i += 1;
      };
    };
  };
  if Equals(equipArea.areaType, gamedataEquipmentArea.ArmsCW) {
    this.UnequipItem(equipAreaIndex, slotIndex);
  };
  this.m_equipment.equipAreas[equipAreaIndex].equipSlots[slotIndex].itemID = itemID;
  transactionSystem = GameInstance.GetTransactionSystem(this.m_owner.GetGame());
  placementSlot = this.GetPlacementSlot(equipAreaIndex, slotIndex);
  if placementSlot == t"AttachmentSlots.WeaponRight" || placementSlot == t"AttachmentSlots.WeaponLeft" {
    weaponRecord = TweakDBInterface.GetWeaponItemRecord(ItemID.GetTDBID(itemID));
    if IsDefined(weaponRecord) && IsDefined(weaponRecord.HolsteredItem()) {
      EquipmentSystemPlayerData.UpdateArmSlot(this.m_owner as PlayerPuppet, itemID, true);
    };
  };
  if placementSlot != t"AttachmentSlots.WeaponRight" && placementSlot != t"AttachmentSlots.WeaponLeft" && placementSlot != t"AttachmentSlots.Consumable" {
    if !transactionSystem.HasItemInSlot(this.m_owner, placementSlot, itemID) {
      transactionSystem.RemoveItemFromSlot(this.m_owner, placementSlot);
      transactionSystem.AddItemToSlot(this.m_owner, placementSlot, itemID);
    };
  };
  if Equals(RPGManager.GetItemType(itemID), gamedataItemType.Clo_Feet) {
    audioEventFootwear = new AudioEvent();
    audioEventFootwear.eventName = n"equipFootwear";
    audioEventFootwear.nameData = RPGManager.GetItemRecord(itemID).MovementSound().AudioMovementName();
    this.m_owner.QueueEvent(audioEventFootwear);
  };
  audioEventFoley = new AudioEvent();
  audioEventFoley.eventName = n"equipItem";
  audioEventFoley.nameData = TweakDBInterface.GetItemRecord(ItemID.GetTDBID(itemID)).AppearanceName();
  this.m_owner.QueueEvent(audioEventFoley);
  paperdollEquipData.equipArea = this.m_equipment.equipAreas[equipAreaIndex];
  paperdollEquipData.equipped = ShouldDisplayHG(itemID); // <- tweaks visibility here
  paperdollEquipData.placementSlot = placementSlot;
  paperdollEquipData.slotIndex = slotIndex;
  this.ApplyEquipGLPs(itemID);
  if itemData.UsesVariants() {
    itemData.AddStatsOnEquip(this.m_owner);
  };
  this.UpdateWeaponWheel();
  this.UpdateQuickWheel();
  this.UpdateEquipmentUIBB(paperdollEquipData);
  i = 0;
  while i < ArraySize(this.m_hotkeys) {
    if this.m_hotkeys[i].IsCompatible(itemData.GetItemType()) {
      this.AssignItemToHotkey(itemData.GetID(), this.m_hotkeys[i].GetHotkey());
    };
    i += 1;
  };
  EquipmentSystem.GetInstance(this.m_owner).Debug_FillESSlotData(slotIndex, this.m_equipment.equipAreas[equipAreaIndex].areaType, itemID, this.m_owner);
  if ItemID.IsValid(currentItem) && currentItem != itemID {
    transactionSystem.OnItemRemovedFromEquipmentSlot(this.m_owner, currentItem);
  };
  if RPGManager.IsItemClothing(itemID) && this.IsSlotHidden(equipArea.areaType) {
    transactionSystem.OnItemAddedToEquipmentSlot(this.m_owner, itemID, n"empty_appearance_default", false);
  } else {
    if RPGManager.IsItemClothing(itemID) && this.IsSlotOverriden(equipArea.areaType) {
      transactionSystem.OnItemAddedToEquipmentSlot(this.m_owner, itemID, EquipmentSystem.GetClothingItemAppearanceName(this.GetSlotOverridenVisualItem(equipArea.areaType)), true);
    } else {
      transactionSystem.OnItemAddedToEquipmentSlot(this.m_owner, itemID);
    };
  };
  if this.IsItemOfCategory(itemID, gamedataItemCategory.Cyberware) || Equals(equipArea.areaType, gamedataEquipmentArea.ArmsCW) {
    this.CheckCyberjunkieAchievement();
  };
  if EquipmentSystem.IsItemCyberdeck(itemID) {
    PlayerPuppet.ChacheQuickHackListCleanup(this.m_owner);
  };
}


// [VehicleComponent]

// Unequip head item on mount
@wrapMethod(VehicleComponent)
protected cb func OnMountingEvent(evt: ref<MountingEvent>) -> Bool {
  wrappedMethod(evt);

  let playerMounted: ref<GameObject> = GameInstance.FindEntityByID(this.GetVehicle().GetGame(), evt.request.lowLevelMountingInfo.childId) as GameObject;
  if IsDefined(playerMounted) {
    EquipmentSystem.GetData(playerMounted).ClearHeadSlot();
  };
}

// Reequip head item on unmount
@wrapMethod(VehicleComponent)
protected cb func OnUnmountingEvent(evt: ref<UnmountingEvent>) -> Bool {
  wrappedMethod(evt);

  let playerMounted: ref<GameObject> = GameInstance.FindEntityByID(this.GetVehicle().GetGame(), evt.request.lowLevelMountingInfo.childId) as GameObject;
  let playerData: ref<EquipmentSystemPlayerData>;
  if IsDefined(playerMounted) {
    playerData = EquipmentSystem.GetData(playerMounted);
    playerData.UnequipHeadSlot();
    playerData.ReequipHeadSlot();
  };
}


// [TakeOverControlSystem]

// Test bald head fix for hacked camera TPP view
@replaceMethod(TakeOverControlSystem)
private final const func EnablePlayerTPPRepresenation(enable: Bool) -> Void {
  let player: ref<PlayerPuppet> = GameInstance.GetPlayerSystem(this.GetGameInstance()).GetLocalPlayerControlledGameObject() as PlayerPuppet;
  let playerData: ref<EquipmentSystemPlayerData>;
  if IsDefined(player) {
    playerData = EquipmentSystem.GetData(player);
    if enable {
      player.QueueEvent(new ActivateTPPRepresentationEvent());
      playerData.UnequipHeadSlot();
      GameInstance.GetAudioSystem(this.GetGameInstance()).SetBDCameraListenerOverride(true);
      GameObjectEffectHelper.StartEffectEvent(player, n"camera_mask");
    } else {
      player.QueueEvent(new DeactivateTPPRepresentationEvent());
      playerData.ReequipHeadSlot();
      GameInstance.GetAudioSystem(this.GetGameInstance()).SetBDCameraListenerOverride(false);
      GameObjectEffectHelper.StopEffectEvent(player, n"camera_mask");
    };
  };
}


// [InteractiveDevice]

// Test bald head fix for mirror TPP view
@wrapMethod(InteractiveDevice)
protected cb func OnPerformedAction(evt: ref<PerformedAction>) -> Bool {
  wrappedMethod(evt);

  let equipmentSystem: ref<EquipmentSystem>;
  let equipmentSystemPlayerData: ref<EquipmentSystemPlayerData>;
  let activePlayer: ref<PlayerPuppet> = GameInstance.GetPlayerSystem(this.GetGame()).GetLocalPlayerControlledGameObject() as PlayerPuppet;
  let actionName: CName = evt.m_action.actionName;
  let unequipCallback: ref<DelayedUnequipCallback>;
  let reequipCallback: ref<DelayedReequipCallback>;

  if IsDefined(activePlayer) {
    equipmentSystem = EquipmentSystem.GetInstance(activePlayer);
    equipmentSystemPlayerData = EquipmentSystem.GetData(activePlayer);
    if Equals(actionName, n"ForceON") {
      // equipmentSystemPlayerData.UnequipHeadSlot();
      unequipCallback = new DelayedUnequipCallback();
      unequipCallback.playerData = equipmentSystemPlayerData;
      GameInstance.GetDelaySystem(this.GetGame()).DelayCallback(unequipCallback, 1);
    } else {
      if Equals(actionName, n"ForceOFF") {
        // equipmentSystemPlayerData.ReequipHeadSlot();
        reequipCallback = new DelayedReequipCallback();
        reequipCallback.playerData = equipmentSystemPlayerData;
        GameInstance.GetDelaySystem(this.GetGame()).DelayCallback(reequipCallback, 3);
      };
    };
  };
}

// // --- TESTING

// public static func PrintPlayerStats(where: String, object: ref<GameObject>) -> Void {
//   let statsSystem: ref<StatsSystem> = GameInstance.GetStatsSystem(object.GetGame());
//   let armorValue: Float = statsSystem.GetStatValue(Cast(object.GetEntityID()), gamedataStatType.Armor);
//   let critChance: Float = statsSystem.GetStatValue(Cast(object.GetEntityID()), gamedataStatType.CritChance);
//   let critDamage: Float = statsSystem.GetStatValue(Cast(object.GetEntityID()), gamedataStatType.CritDamage);
//   LogChannel(n"DEBUG", s"Stats from " + where + " - armor: " + FloatToString(armorValue) + ", crit chance: " + FloatToString(critChance) + ", crit damage: " + FloatToString(critDamage));
// }

// @wrapMethod(PlayerPuppet)
// protected cb func OnAction(action: ListenerAction, consumer: ListenerActionConsumer) -> Bool {
//   wrappedMethod(action, consumer);
//   PrintPlayerStats("NOW", this);
// }
