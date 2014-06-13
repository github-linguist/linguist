#==============================================================================
# 
# Бе Yanfly Engine Ace - Visual Battlers v1.01
# -- Last Updated: 2012.07.24
# -- Level: Easy
# -- Requires: n/a
#
# Бе Modified by:
# -- Yami
# -- Kread-Ex
# -- Archeia_Nessiah
#==============================================================================

$imported = {} if $imported.nil?
$imported["YEA-VisualBattlers"] = true

#==============================================================================
# Бе Updates
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# 2012.12.18 - Added preset views and able to change direction in-game.
# 2012.07.24 - Finished Script.
# 2012.01.05 - Started Script.
# 
#==============================================================================
# Бе Introduction
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# This script provides a visual for all actors by default charsets. The actions
# and movements are alike Final Fantasy 1, only move forward and backward when 
# start and finish actions.
# 
#==============================================================================
# Бе Instructions
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# To change the player direction in-game, use the snippet below in a script 
# call: 
# 
# $game_system.party_direction = n
#
# To install this script, open up your script editor and copy/paste this script
# to an open slot below Бе Materials but above Бе Main. Remember to save.
# 
#==============================================================================
# Бе Compatibility
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# This script is made strictly for RPG Maker VX Ace. It is highly unlikely that
# it will run with RPG Maker VX without adjusting.
# 
#==============================================================================

module YEA
  module VISUAL_BATTLERS
    
    #=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
    # - Party Location Setting -
    #=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
    # These settings are adjusted for Party Location. Each Actor will have
    # coordinates calculated by below formula. There are two samples coordinates
    # below, change PARTY_DIRECTION to the base index you want to use.
    #=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
    PARTY_DIRECTION = 6 # This direction is opposite from actual direction.
    
    PARTY_LOCATION_BASE_COORDINATES ={
    # Index => [base_x, base_y, mod_x, mod_y],
          2 => [   250,    290,    40,     0], #UP
          4 => [   150,    280,    20,   -20], #LEFT
          3 => [   460,    280,    30,   -10], #RIGHT
          6 => [   460,    230,    20,    20], #DEFAULT RIGHT
          8 => [   260,    230,    40,     0], #DOWN
    } # Do not remove this.
    
    PARTY_LOCATION_FORMULA_X = "base_x + index * mod_x"
    PARTY_LOCATION_FORMULA_Y = "base_y + index * mod_y"
    
  end # VISUAL_BATTLERS
end # YEA

#==============================================================================
#  Бе  Editting anything past this point may potentially result in causing
# computer damage, incontinence, explosion of user's head, coma, death, and/or
# halitosis so edit at your own risk.
#==============================================================================

#==============================================================================
# ? Бе  Direction
#==============================================================================

module Direction
  
  #--------------------------------------------------------------------------
  # self.correct
  #--------------------------------------------------------------------------
  def self.correct(direction)
    case direction
    when 1; return 4
    when 3; return 6
    when 7; return 4
    when 9; return 6
    else; return direction
    end
  end
  
  #--------------------------------------------------------------------------
  # self.opposite
  #--------------------------------------------------------------------------
  def self.opposite(direction)
    case direction
    when 1; return 6
    when 2; return 8
    when 3; return 4
    when 4; return 6
    when 6; return 4
    when 7; return 6
    when 8; return 2
    when 9; return 4
    else; return direction
    end
  end
  
end # Direction

#==============================================================================
# ? Бе  Game_System
#==============================================================================

class Game_System; attr_accessor :party_direction; end

#==============================================================================
# ? Бе  Game_BattleCharacter
#==============================================================================

class Game_BattleCharacter < Game_Character
  
  #--------------------------------------------------------------------------
  # initialize
  #--------------------------------------------------------------------------
  def initialize(actor)
    super()
    setup_actor(actor)
    @move_x_rate = 0
    @move_y_rate = 0
  end
  
  #--------------------------------------------------------------------------
  # setup_actor
  #--------------------------------------------------------------------------
  def setup_actor(actor)
    @actor = actor
    @step_anime = true
    set_graphic(@actor.character_name, @actor.character_index)
    setup_coordinates
    dr = $game_system.party_direction || YEA::VISUAL_BATTLERS::PARTY_DIRECTION
    direction = Direction.opposite(dr)
    set_direction(Direction.correct(direction))
  end
  
  #--------------------------------------------------------------------------
  # sprite=
  #--------------------------------------------------------------------------
  def sprite=(sprite)
    @sprite = sprite
  end
  
  #--------------------------------------------------------------------------
  # setup_coordinates
  #--------------------------------------------------------------------------
  def setup_coordinates
    location = ($game_system.party_direction ||
    YEA::VISUAL_BATTLERS::PARTY_DIRECTION)
    base_x = YEA::VISUAL_BATTLERS::PARTY_LOCATION_BASE_COORDINATES[location][0]
    base_y = YEA::VISUAL_BATTLERS::PARTY_LOCATION_BASE_COORDINATES[location][1]
    mod_x = YEA::VISUAL_BATTLERS::PARTY_LOCATION_BASE_COORDINATES[location][2]
    mod_y = YEA::VISUAL_BATTLERS::PARTY_LOCATION_BASE_COORDINATES[location][3]
    @actor.screen_x = eval(YEA::VISUAL_BATTLERS::PARTY_LOCATION_FORMULA_X)
    @actor.screen_y = eval(YEA::VISUAL_BATTLERS::PARTY_LOCATION_FORMULA_Y)
    @actor.origin_x = @actor.screen_x
    @actor.origin_y = @actor.screen_y
    @actor.create_move_to(screen_x, screen_y, 1)
  end
  
  #--------------------------------------------------------------------------
  # index
  #--------------------------------------------------------------------------
  def index
    return @actor.index
  end
  
  #--------------------------------------------------------------------------
  # screen_x
  #--------------------------------------------------------------------------
  def screen_x
    return @actor.screen_x
  end
  
  #--------------------------------------------------------------------------
  # screen_y
  #--------------------------------------------------------------------------
  def screen_y
    return @actor.screen_y
  end
  
  #--------------------------------------------------------------------------
  # screen_z
  #--------------------------------------------------------------------------
  def screen_z
    return @actor.screen_z
  end
  
end # Game_BattleCharacter

#==============================================================================
# ? Бе  Game_Battler
#==============================================================================

class Game_Battler < Game_BattlerBase
  
  #--------------------------------------------------------------------------
  # public instance variables
  #--------------------------------------------------------------------------
  attr_accessor :moved_back
  attr_accessor :origin_x
  attr_accessor :origin_y
  attr_accessor :screen_x
  attr_accessor :screen_y
  attr_accessor :started_turn
  
  #--------------------------------------------------------------------------
  # alias method: execute_damage
  #--------------------------------------------------------------------------
  alias game_battler_execute_damage_vb execute_damage
  def execute_damage(user)
    game_battler_execute_damage_vb(user)
    if @result.hp_damage > 0
      move_backward(24, 6) unless @moved_back
      @moved_back = true
    end
  end
  
  #--------------------------------------------------------------------------
  # face_opposing_party
  #--------------------------------------------------------------------------
  def face_opposing_party
    direction = ($game_system.party_direction ||
    YEA::VISUAL_BATTLERS::PARTY_DIRECTION)
    character.set_direction(Direction.correct(direction)) rescue 0
  end
  
  #--------------------------------------------------------------------------
  # new method: face_coordinate
  #--------------------------------------------------------------------------
  def face_coordinate(destination_x, destination_y)
    x1 = Integer(@screen_x)
    x2 = Integer(destination_x)
    y1 = Graphics.height - Integer(@screen_y)
    y2 = Graphics.height - Integer(destination_y)
    return if x1 == x2 and y1 == y2
    #---
    angle = Integer(Math.atan2((y2-y1),(x2-x1)) * 1800 / Math::PI)
    if (0..225) === angle or (-225..0) === angle
      direction = 6
    elsif (226..675) === angle
      direction = 9
    elsif (676..1125) === angle
      direction = 8
    elsif (1126..1575) === angle
      direction = 7
    elsif (1576..1800) === angle or (-1800..-1576) === angle
      direction = 4
    elsif (-1575..-1126) === angle
      direction = 1
    elsif (-1125..-676) === angle
      direction = 2
    elsif (-675..-226) === angle
      direction = 3
    end
    #---
    character.set_direction(Direction.correct(direction)) rescue 0
  end
  
  #--------------------------------------------------------------------------
  # create_move_to
  #--------------------------------------------------------------------------
  def create_move_to(destination_x, destination_y, frames = 12)
    @destination_x = destination_x
    @destination_y = destination_y
    frames = [frames, 1].max
    @move_x_rate = [(@screen_x - @destination_x).abs / frames, 2].max
    @move_y_rate = [(@screen_y - @destination_y).abs / frames, 2].max
  end
  
  #--------------------------------------------------------------------------
  # update_move_to
  #--------------------------------------------------------------------------
  def update_move_to
    @move_x_rate = 0 if @screen_x == @destination_x || @move_x_rate.nil?
    @move_y_rate = 0 if @screen_y == @destination_y || @move_y_rate.nil?
    value = [(@screen_x - @destination_x).abs, @move_x_rate].min
    @screen_x += (@destination_x > @screen_x) ? value : -value
    value = [(@screen_y - @destination_y).abs, @move_y_rate].min
    @screen_y += (@destination_y > @screen_y) ? value : -value
  end
  
  #--------------------------------------------------------------------------
  # move_forward
  #--------------------------------------------------------------------------
  def move_forward(distance = 24, frames = 12)
    direction = forward_direction
    move_direction(direction, distance, frames)
  end
  
  #--------------------------------------------------------------------------
  # move_backward
  #--------------------------------------------------------------------------
  def move_backward(distance = 24, frames = 12)
    direction = Direction.opposite(forward_direction)
    move_direction(direction, distance, frames)
  end
  
  #--------------------------------------------------------------------------
  # move_direction
  #--------------------------------------------------------------------------
  def move_direction(direction, distance = 24, frames = 12)
    case direction
    when 1; move_x = distance / -2; move_y = distance /  2
    when 2; move_x = distance *  0; move_y = distance *  1
    when 3; move_x = distance / -2; move_y = distance /  2
    when 4; move_x = distance * -1; move_y = distance *  0
    when 6; move_x = distance *  1; move_y = distance *  0
    when 7; move_x = distance / -2; move_y = distance / -2
    when 8; move_x = distance *  0; move_y = distance * -1
    when 9; move_x = distance /  2; move_y = distance / -2
    else; return
    end
    destination_x = @screen_x + move_x
    destination_y = @screen_y + move_y
    create_move_to(destination_x, destination_y, frames)
  end
  
  #--------------------------------------------------------------------------
  # forward_direction
  #--------------------------------------------------------------------------
  def forward_direction
    return ($game_system.party_direction ||
    YEA::VISUAL_BATTLERS::PARTY_DIRECTION)
  end
  
  #--------------------------------------------------------------------------
  # move_origin
  #--------------------------------------------------------------------------
  def move_origin
    create_move_to(@origin_x, @origin_y)
    face_coordinate(@origin_x, @origin_y)
    @moved_back = false
  end
  
  #--------------------------------------------------------------------------
  # moving?
  #--------------------------------------------------------------------------
  def moving?
    return false if dead? || !exist?
    return @move_x_rate != 0 || @move_y_rate != 0
  end
  
end # Game_Battler

#==============================================================================
# ? Бе  Game_Actor
#==============================================================================

class Game_Actor < Game_Battler
  
  #--------------------------------------------------------------------------
  # overwrite method: use_sprite?
  #--------------------------------------------------------------------------
  def use_sprite?
    return true
  end
  
  #--------------------------------------------------------------------------
  # new method: screen_x
  #--------------------------------------------------------------------------
  def screen_x
    return @screen_x rescue 0
  end
  
  #--------------------------------------------------------------------------
  # new method: screen_y
  #--------------------------------------------------------------------------
  def screen_y
    return @screen_y rescue 0
  end
  
  #--------------------------------------------------------------------------
  # new method: screen_z
  #--------------------------------------------------------------------------
  def screen_z
    return 100
  end
  
  #--------------------------------------------------------------------------
  # new method: sprite
  #--------------------------------------------------------------------------
  def sprite
    index = $game_party.battle_members.index(self)
    return SceneManager.scene.spriteset.actor_sprites[index]
  end
  
  #--------------------------------------------------------------------------
  # new method: character
  #--------------------------------------------------------------------------
  def character
    return sprite.character_base
  end
  
  #--------------------------------------------------------------------------
  # face_opposing_party
  #--------------------------------------------------------------------------
  def face_opposing_party
    dr = $game_system.party_direction || YEA::VISUAL_BATTLERS::PARTY_DIRECTION
    direction = Direction.opposite(dr)
    character.set_direction(Direction.correct(direction)) rescue 0
  end
  
  #--------------------------------------------------------------------------
  # forward_direction
  #--------------------------------------------------------------------------
  def forward_direction
    return Direction.opposite(($game_system.party_direction ||
    YEA::VISUAL_BATTLERS::PARTY_DIRECTION))
  end
  
end # Game_Actor

#==============================================================================
# ? Бе  Game_Enemy
#==============================================================================

class Game_Enemy < Game_Battler
  
  #--------------------------------------------------------------------------
  # new method: sprite
  #--------------------------------------------------------------------------
  def sprite
    return SceneManager.scene.spriteset.enemy_sprites.reverse[self.index]
  end
  
  #--------------------------------------------------------------------------
  # new method: character
  #--------------------------------------------------------------------------
  def character
    return sprite
  end
  
end # Game_Enemy

#==============================================================================
# ? Бе  Game_Troop
#==============================================================================

class Game_Troop < Game_Unit
  
  #--------------------------------------------------------------------------
  # alias method: setup
  #--------------------------------------------------------------------------
  alias game_troop_setup_vb setup
  def setup(troop_id)
    game_troop_setup_vb(troop_id)
    set_coordinates
  end
  
  #--------------------------------------------------------------------------
  # new method: set_coordinates
  #--------------------------------------------------------------------------
  def set_coordinates
    for member in members
      member.origin_x = member.screen_x
      member.origin_y = member.screen_y
      member.create_move_to(member.screen_x, member.screen_y, 1)
    end
  end
  
end # Game_Troop

#==============================================================================
# ? Бе  Sprite_Battler
#==============================================================================

class Sprite_Battler < Sprite_Base
  
  #--------------------------------------------------------------------------
  # public instance_variable
  #--------------------------------------------------------------------------
  attr_accessor :character_base
  attr_accessor :character_sprite
  
  #--------------------------------------------------------------------------
  # alias method: dispose
  #--------------------------------------------------------------------------
  alias sprite_battler_dispose_vb dispose
  def dispose
    dispose_character_sprite
    sprite_battler_dispose_vb
  end
  
  #--------------------------------------------------------------------------
  # new method: dispose_character_sprite
  #--------------------------------------------------------------------------
  def dispose_character_sprite
    @character_sprite.dispose unless @character_sprite.nil?
  end
  
  #--------------------------------------------------------------------------
  # alias method: update
  #--------------------------------------------------------------------------
  alias sprite_battler_update_vb update
  def update
    sprite_battler_update_vb
    return if @battler.nil?
    update_move_to
    update_character_base
    update_character_sprite
  end
  
  #--------------------------------------------------------------------------
  # new method: update_character_base
  #--------------------------------------------------------------------------
  def update_character_base
    return if @character_base.nil?
    @character_base.update
  end
  
  #--------------------------------------------------------------------------
  # new method: update_character_sprite
  #--------------------------------------------------------------------------
  def update_character_sprite
    return if @character_sprite.nil?
    @character_sprite.update
  end
  
  #--------------------------------------------------------------------------
  # new method: update_move_to
  #--------------------------------------------------------------------------
  def update_move_to
    @battler.update_move_to
  end
  
  #--------------------------------------------------------------------------
  # new method: moving?
  #--------------------------------------------------------------------------
  def moving?
    return false if @battler.nil?
    return @battler.moving?
  end
  
end # Sprite_Battler

#==============================================================================
# ? Бе  Sprite_BattleCharacter
#==============================================================================

class Sprite_BattleCharacter < Sprite_Character
  
  #--------------------------------------------------------------------------
  # initialize
  #--------------------------------------------------------------------------
  def initialize(viewport, character = nil)
    super(viewport, character)
    character.sprite = self
  end
  
end # Sprite_BattleCharacter

#==============================================================================
# ? Бе  Spriteset_Battle
#==============================================================================

class Spriteset_Battle
  
  #--------------------------------------------------------------------------
  # public instance_variable
  #--------------------------------------------------------------------------
  attr_accessor :actor_sprites
  attr_accessor :enemy_sprites
  
  #--------------------------------------------------------------------------
  # overwrite method: create_actors
  #--------------------------------------------------------------------------
  def create_actors
    total = $game_party.max_battle_members
    @current_party = $game_party.battle_members.clone
    @actor_sprites = Array.new(total) { Sprite_Battler.new(@viewport1) }
    for actor in $game_party.battle_members
      @actor_sprites[actor.index].battler = actor
      create_actor_sprite(actor)
    end
  end
  
  #--------------------------------------------------------------------------
  # new method: create_actor_sprite
  #--------------------------------------------------------------------------
  def create_actor_sprite(actor)
    character = Game_BattleCharacter.new(actor)
    character_sprite = Sprite_BattleCharacter.new(@viewport1, character)
    @actor_sprites[actor.index].character_base = character
    @actor_sprites[actor.index].character_sprite = character_sprite
    actor.face_opposing_party
  end
  
  #--------------------------------------------------------------------------
  # alias method: update_actors
  #--------------------------------------------------------------------------
  alias spriteset_battle_update_actors_vb update_actors
  def update_actors
    if @current_party != $game_party.battle_members
      dispose_actors
      create_actors
    end
    spriteset_battle_update_actors_vb
  end
  
  #--------------------------------------------------------------------------
  # new method: moving?
  #--------------------------------------------------------------------------
  def moving?
    return battler_sprites.any? {|sprite| sprite.moving? }
  end
  
end # Spriteset_Battle

#==============================================================================
# ? Бе  Scene_Battle
#==============================================================================

class Scene_Battle < Scene_Base
  
  #--------------------------------------------------------------------------
  # public instance variables
  #--------------------------------------------------------------------------
  attr_accessor :spriteset
  
  #--------------------------------------------------------------------------
  # alias method: process_action_end
  #--------------------------------------------------------------------------
  alias scene_battle_process_action_end_vb process_action_end
  def process_action_end
    start_battler_move_origin
    scene_battle_process_action_end_vb
  end
  
  #--------------------------------------------------------------------------
  # alias method: execute_action
  #--------------------------------------------------------------------------
  alias scene_battle_execute_action_vb execute_action
  def execute_action
    start_battler_move_forward
    scene_battle_execute_action_vb
  end
  
  #--------------------------------------------------------------------------
  # new method: start_battler_move_forward
  #--------------------------------------------------------------------------
  def start_battler_move_forward
    return if @subject.started_turn
    @subject.started_turn = true
    @subject.move_forward
    wait_for_moving
  end
  
  #--------------------------------------------------------------------------
  # new method: start_battler_move_origin
  #--------------------------------------------------------------------------
  def start_battler_move_origin
    @subject.started_turn = nil
    move_battlers_origin
    wait_for_moving
    @subject.face_opposing_party rescue 0
  end
  
  #--------------------------------------------------------------------------
  # new method: move_battlers_origin
  #--------------------------------------------------------------------------
  def move_battlers_origin
    for member in all_battle_members
      next if member.dead?
      next unless member.exist?
      member.move_origin
    end
  end
  
  #--------------------------------------------------------------------------
  # new method: wait_for_moving
  #--------------------------------------------------------------------------
  def wait_for_moving
    update_for_wait
    update_for_wait while @spriteset.moving?
  end
  
end # Scene_Battle

#==============================================================================
# 
#  Бе  End of File
# 
#==============================================================================
