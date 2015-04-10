@marina-text: #576ddf; // also swimming_pool
@wetland-text: darken(#017fff, 10%); /* Also for marsh */
@mud-text: darken(#aea397, 20%);
@shop-icon: #ac39ac;
@transportation-icon: #0092da;
@transportation-text: #0066ff;
@airtransport: #8461C4;

@landcover-font-size: 10;
@landcover-font-size-big: 12;
@landcover-font-size-bigger: 15;
@landcover-wrap-width-size: 25;
@landcover-wrap-width-size-big: 35;
@landcover-wrap-width-size-bigger: 45;
@landcover-face-name: @oblique-fonts;

@standard-wrap-width: 30;

.points {
  [feature = 'tourism_alpine_hut'][zoom >= 13] {
    point-file: url('symbols/alpinehut.p.16.png');
    point-placement: interior;
  }

  [feature = 'amenity_shelter'][zoom >= 16] {
    point-file: url('symbols/shelter2.p.16.png');
    point-placement: interior;
  }

  [feature = 'amenity_atm'][zoom >= 17] {
    point-file: url('symbols/atm2.p.16.png');
    point-placement: interior;
  }

  [feature = 'amenity_bank'][zoom >= 17] {
    point-file: url('symbols/bank2.p.16.png');
    point-placement: interior;
  }

  [feature = 'amenity_bar'][zoom >= 17] {
    point-file: url('symbols/bar.p.20.png');
    point-placement: interior;
  }

  [feature = 'amenity_bicycle_rental'][zoom >= 17] {
    point-file: url('symbols/rental_bicycle.p.20.png');
    point-placement: interior;
  }

  [feature = 'highway_bus_stop'] {
    [zoom >= 16] {
      marker-file: url('symbols/square.svg');
      marker-fill: @transportation-icon;
      marker-placement: interior;
      marker-width: 6;
    }
    [zoom >= 17] {
      marker-file: url('symbols/bus_stop.p.12.png');
      marker-width: 12;
    }
  }

  [feature = 'amenity_bus_station'][zoom >= 16] {
    point-file: url('symbols/bus_station.n.16.png');
    point-placement: interior;
  }

  [feature = 'highway_traffic_signals'][zoom >= 17] {
    marker-file: url('symbols/traffic_light.svg');
    marker-fill: #0a0a0a;
    marker-placement: interior;
  }

  [feature = 'amenity_cafe'][zoom >= 17] {
    point-file: url('symbols/cafe.p.16.png');
    point-placement: interior;
  }

  [feature = 'tourism_camp_site'][zoom >= 16] {
    point-file: url('symbols/camping.n.16.png');
    point-placement: interior;
  }

  [feature = 'highway_ford'][zoom >= 16] {
    point-file: url('symbols/transport_ford.p.16.png');
    point-placement: interior;
  }

  [feature = 'tourism_caravan_site'][zoom >= 16] {
    point-file: url('symbols/caravan_park.p.24.png');
    point-placement: interior;
  }

  [feature = 'amenity_car_sharing'][zoom >= 16] {
    point-file: url('symbols/car_share.p.16.png');
    point-placement: interior;
  }

  [feature = 'tourism_chalet'][zoom >= 17] {
    point-file: url('symbols/chalet.p.16.png');
    point-placement: interior;
  }

  [feature = 'amenity_cinema'][zoom >= 16] {
    point-file: url('symbols/cinema.p.24.png');
    point-placement: interior;
  }

  [feature = 'amenity_fire_station'][zoom >= 16] {
    point-file: url('symbols/firestation.p.16.png');
    point-placement: interior;
  }

  [feature = 'amenity_fuel'][zoom >= 17] {
    point-file: url('symbols/fuel.p.16.png');
    point-placement: interior;
  }

  [feature = 'tourism_guest_house'][zoom >= 17] {
    point-file: url('symbols/guest_house.p.16.png');
    point-placement: interior;
  }

  [feature = 'amenity_hospital'][zoom >= 15] {
    point-file: url('symbols/hospital.p.16.png');
    point-placement: interior;
  }

  [feature = 'tourism_hostel'][zoom >= 17] {
    point-file: url('symbols/hostel.p.20.png');
    point-placement: interior;
  }

  [feature = 'tourism_hotel'][zoom >= 17] {
    point-file: url('symbols/hotel2.p.20.png');
    point-placement: interior;
  }

  [feature = 'tourism_motel'][zoom >= 17] {
    point-file: url('symbols/motel.p.20.png');
    point-placement: interior;
  }

  [feature = 'tourism_information'][zoom >= 17] {
    point-file: url('symbols/information.p.16.png');
    point-placement: interior;
  }

  [feature = 'amenity_embassy'][zoom >= 17] {
    point-file: url('symbols/embassy.png');
    point-placement: interior;
  }

  [feature = 'amenity_library'][zoom >= 16] {
    point-file: url('symbols/library.p.20.png');
    point-placement: interior;
  }

  [feature = 'amenity_courthouse'][zoom > 16] {
    point-file: url('symbols/amenity_court.p.20.png');
    point-placement: interior;
  }

  [feature = 'waterway_lock'],
  [feature = 'lock_yes'] {
    [zoom >= 15] {
      marker-fill: #969494;
      marker-width: 9;
      marker-line-width: 0;
      marker-placement: interior;
    }
  }

  [feature = 'man_made_mast'][zoom >= 17] {
    point-file: url('symbols/communications.p.20.png');
    point-placement: interior;
  }

  [feature = 'tourism_museum'][zoom >= 16] {
    point-file: url('symbols/museum.p.16.png');
    point-placement: interior;
  }

  [feature = 'amenity_parking'][zoom >= 16] {
    marker-file: url('symbols/parking.svg');
    marker-placement: interior;
    marker-clip: false;
    marker-fill: @transportation-icon;
    [access != ''][access != 'public'][access != 'yes'] {
      marker-opacity: 0.33;
    }
  }

  [feature = 'amenity_pharmacy'][zoom >= 17] {
    point-file: url('symbols/pharmacy.p.16.png');
    point-placement: interior;
  }

  [feature = 'amenity_place_of_worship'][zoom >= 16] {
    point-file: url('symbols/place_of_worship3.p.16.png');
    point-placement: interior;
    [religion = 'christian'] {
      point-file: url('symbols/christian3.p.14.png');
      [denomination = 'jehovahs_witness']{
        point-file: url('symbols/place_of_worship3.p.16.png');
      }
    }
    [religion = 'muslim'] {
      point-file: url('symbols/islamic3.p.16.png');
    }
    [religion = 'sikh'] {
      point-file: url('symbols/sikh3.p.16.png');
    }
    [religion = 'jewish'] {
      point-file: url('symbols/jewish3.p.16.png');
    }
    [religion = 'hindu'] {
      point-file: url('symbols/hindu.png');
    }
    [religion = 'buddhist'] {
      point-file: url('symbols/buddhist.png');
    }
    [religion = 'shinto'] {
      point-file: url('symbols/shinto.png');
    }
    [religion = 'taoist'] {
      point-file: url('symbols/taoist.png');
    }
  }

  [feature = 'amenity_police'][zoom >= 16] {
    point-file: url('symbols/police.p.16.png');
    point-placement: interior;
  }

  [feature = 'amenity_post_box'][zoom >= 17] {
    point-file: url('symbols/post_box.p.16.png');
    point-placement: interior;
  }

  [feature = 'amenity_post_office'][zoom >= 17] {
    point-file: url('symbols/post_office.p.16.png');
    point-placement: interior;
  }

  [feature = 'amenity_pub'][zoom >= 17] {
    point-file: url('symbols/pub.p.16.png');
    point-placement: interior;
  }

  [feature = 'amenity_biergarten'][zoom >= 17] {
    point-file: url('symbols/biergarten.p.16.png');
    point-placement: interior;
  }

  [feature = 'amenity_recycling'][zoom >= 16] {
    point-file: url('symbols/recycling.p.16.png');
    point-placement: interior;
  }

  [feature = 'amenity_restaurant'][zoom >= 17] {
    point-file: url('symbols/restaurant.p.16.png');
    point-placement: interior;
  }

  [feature = 'amenity_fast_food'][zoom >= 17] {
    point-file: url('symbols/fast_food.png');
    point-placement: interior;
  }

  [feature = 'amenity_telephone'][zoom >= 17] {
    point-file: url('symbols/telephone.p.16.png');
    point-placement: interior;
  }

  [feature = 'amenity_emergency_phone'][zoom >= 17] {
    point-file: url('symbols/sosphone.png');
    point-placement: interior;
  }

  [feature = 'amenity_theatre'][zoom >= 16] {
    point-file: url('symbols/theatre.p.20.png');
    point-placement: interior;
  }

  [feature = 'amenity_toilets'][zoom >= 17] {
    point-file: url('symbols/toilets.p.20.png');
    point-placement: interior;
  }

  [feature = 'amenity_drinking_water'][zoom >= 17] {
    point-file: url('symbols/food_drinkingtap.p.20.png');
    point-placement: interior;
  }

  [feature = 'amenity_prison'][zoom >= 17] {
    point-file: url('symbols/amenity_prison.p.20.png');
    point-placement: interior;
  }

  [feature = 'tourism_viewpoint'][zoom >= 16] {
    point-file: url('symbols/view_point.p.16.png');
    point-placement: interior;
  }

  [feature = 'man_made_water_tower'][zoom >= 17] {
    point-file: url('symbols/tower_water.p.20.png');
    point-placement: interior;
  }

  [feature = 'historic_memorial'][zoom >= 17] {
    point-file: url('symbols/tourist_memorial.p.20.png');
    point-placement: interior;
  }

  [feature = 'historic_archaeological_site'][zoom >= 16] {
    point-file: url('symbols/tourist_archaeological2.glow.24.png');
    point-placement: interior;
  }

  [feature = 'shop_other'][zoom >= 17] {
    marker-fill: @shop-icon;
    marker-width: 6;
    marker-line-width: 0;
    marker-placement: interior;
    marker-clip: false;
  }

  [feature = 'shop_supermarket'][zoom >= 16] {
    marker-file: url('symbols/shop_supermarket.svg');
    marker-placement: interior;
    marker-clip: false;
    marker-fill: @shop-icon;
  }

  [feature = 'shop_bakery'][zoom >= 17] {
    marker-file: url('symbols/shop_bakery.p.16.png');
    marker-placement: interior;
    marker-clip: false;
  }

  [feature = 'shop_butcher'][zoom >= 17] {
    marker-file: url('symbols/shop_butcher.png');
    marker-placement: interior;
    marker-clip: false;
  }

  [feature = 'shop_clothes'],
  [feature = 'shop_fashion'] {
    [zoom >= 17] {
      marker-file: url('symbols/shop_clothes.svg');
      marker-placement: interior;
      marker-clip: false;
      marker-fill: @shop-icon;
    }
  }

  [feature = 'shop_convenience'][zoom >= 17] {
    marker-file: url('symbols/shop_convenience.svg');
    marker-placement: interior;
    marker-clip: false;
    marker-fill: @shop-icon;
  }

  [feature = 'shop_department_store'][zoom >= 16] {
    point-file: url('symbols/department_store.p.16.png');
    point-placement: interior;
  }

  [feature = 'shop_doityourself'][zoom >= 17] {
    marker-file: url('symbols/shop_diy.p.16.png');
    marker-placement: interior;
    marker-clip: false;
  }

  [feature = 'shop_florist'][zoom >= 17] {
    marker-file: url('symbols/florist.p.16.png');
    marker-placement: interior;
    marker-clip: false;
  }

  [feature = 'shop_hairdresser'][zoom >= 17] {
    marker-file: url('symbols/shop_hairdresser.p.16.png');
    marker-placement: interior;
    marker-clip: false;
  }

  [feature = 'shop_car'][zoom >= 17] {
    marker-file: url('symbols/shop_car.svg');
    marker-placement: interior;
    marker-clip: false;
    marker-fill: @shop-icon;
  }

  [feature = 'shop_car_repair'][zoom >= 17] {
    marker-file: url('symbols/shopping_car_repair.p.16.png');
    marker-placement: interior;
    marker-clip: false;
  }

  [feature = 'shop_bicycle'][zoom >= 17] {
    marker-file: url('symbols/shopping_bicycle.p.16.png');
    marker-placement: interior;
    marker-clip: false;
  }

  [feature = 'leisure_playground'][zoom >= 17] {
    point-file: url('symbols/playground.p.20.png');
    point-placement: interior;
  }

  [feature = 'tourism_picnic_site'][zoom >= 16] {
    point-file: url('symbols/picnic.p.16.png');
    point-placement: interior;
  }

  [feature = 'leisure_picnic_table'][zoom >= 17] {
    point-file: url('symbols/picnic.p.16.png');
    point-placement: interior;
  }

  [feature = 'leisure_slipway'][zoom >= 17] {
    point-file: url('symbols/transport_slipway.p.20.png');
    point-placement: interior;
  }

  [feature = 'aeroway_helipad'][zoom >= 16]::aeroway {
    marker-file: url('symbols/helipad.svg');
    marker-clip: false;
    marker-fill: @airtransport;
  }

  [feature = 'aeroway_aerodrome'][zoom >= 10][zoom < 14]::aeroway {
    marker-file: url('symbols/aerodrome.svg');
    marker-clip: false;
    marker-fill: @airtransport;
  }

  [feature = 'man_made_lighthouse'][zoom >= 15]::man_made {
    point-file: url('symbols/lighthouse.p.20.png');
    point-placement: interior;
  }

  [feature = 'natural_peak'][zoom >= 11]::natural {
    marker-file: url('symbols/peak.svg');
    marker-fill: #d08f55;
    marker-placement: interior;
  }

  [feature = 'natural_volcano'][zoom >= 11]::natural {
    marker-file: url('symbols/peak.svg');
    marker-fill: #d40000;
    marker-placement: interior;
  }

  [feature = 'natural_saddle'][zoom >= 15]::natural {
    marker-file: url('symbols/saddle.svg');
    marker-fill: #d08f55;
    marker-placement: interior;
  }

  [feature = 'natural_cave_entrance'][zoom >= 15]::natural {
    point-file: url('symbols/poi_cave.p.16.png');
    point-placement: interior;
  }

  [feature = 'natural_spring'][zoom >= 14]::natural {
    marker-file: url('symbols/spring.svg');
    marker-placement: interior;
  }

  [feature = 'natural_tree'][zoom >= 16]::natural {
    marker-placement: interior;
    marker-ignore-placement: true;
    marker-line-width: 0;
    marker-width: 3;
    marker-fill: #239c45;
    [zoom >= 17] {
      marker-line-width: 1;
      marker-line-color: #8ef2ab;
      marker-width: 4;
    }
  }

  [feature = 'power_generator']['generator:source' = 'wind']::power,
  [feature = 'power_generator'][power_source = 'wind']::power {
    [zoom >= 15] {
      point-file: url('symbols/power_wind.png');
      point-placement: interior;
    }
  }

  [feature = 'man_made_windmill'][zoom >= 16]::man_made {
    point-file: url('symbols/windmill.png');
    point-placement: interior;
  }

  [feature = 'man_made_mast'][zoom >= 17]::man_made {
    point-file: url('symbols/communications.p.20.png');
    point-placement: interior;
  }
}

.amenity-low-priority {
  [railway = 'level_crossing'][zoom >= 14]::railway {
    point-file: url('symbols/level_crossing.svg');
    point-placement: interior;
    [zoom >= 16] {
      point-file: url('symbols/level_crossing2.svg');
    }
  }

  [highway = 'mini_roundabout'][zoom >= 16]::highway {
    marker-file: url('symbols/mini_roundabout.svg');
    marker-placement: interior;
  }

  [barrier = 'gate']::barrier {
    [zoom >= 16] {
      marker-file: url('symbols/gate.svg');
      marker-placement: interior;
    }
  }

  [barrier = 'lift_gate'][zoom >= 16]::barrier {
    marker-file: url('symbols/liftgate.svg');
    marker-fill: #3f3f3f;
    marker-placement: interior
  }

  [barrier = 'bollard'],
  [barrier = 'block'] {
    [zoom >= 16] {
      marker-width: 3;
      marker-line-width: 0;
      marker-fill: #7d7c7c;
      marker-placement: interior;

      [zoom >= 18] {
        marker-width: 4;
      }
    }
  }
}

.text-low-zoom[zoom < 10],
.text[zoom >= 10] {
  [feature = 'place_island'][zoom >= 7][way_pixels > 3000],
  [feature = 'place_island'][zoom >= 16],
  [feature = 'place_islet'][zoom >= 14][way_pixels > 3000],
  [feature = 'place_islet'][zoom >= 17] {
    text-name: "[name]";
    text-fill: #000;
    text-size: 10;
    [way_pixels > 12000] { text-size: 12; }
    [way_pixels > 48000] { text-size: 15; }
    text-face-name: @oblique-fonts;
    text-halo-radius: 1;
    text-wrap-width: @standard-wrap-width;
    text-placement: interior;
  }

  [feature = 'amenity_pub'],
  [feature = 'amenity_restaurant'],
  [feature = 'amenity_cafe'],
  [feature = 'amenity_fast_food'],
  [feature = 'amenity_biergarten'],
  [feature = 'amenity_bar'] {
    [zoom >= 17] {
      text-name: "[name]";
      text-fill: #734a08;
      text-size: 10;
      text-dy: 11;
      text-face-name: @bold-fonts;
      text-halo-radius: 1;
      text-wrap-width: @standard-wrap-width;
      text-placement: interior;
      [feature = 'amenity_bar']{
        text-dy: 13;
      }
    }
  }

  [feature = 'amenity_library'],
  [feature = 'amenity_theatre'],
  [feature = 'amenity_courthouse'],
  [feature = 'amenity_cinema'] {
    [zoom >= 17] {
      text-name: "[name]";
      text-size: 10;
      text-fill: #734a08;
      text-dy: 13;
      text-face-name: @bold-fonts;
      text-halo-radius: 1;
      text-wrap-width: @standard-wrap-width;
      text-placement: interior;
      [feature = 'amenity_cinema'] {
        text-dy: 15;
      }
    }
  }

  [feature = 'amenity_parking'][zoom >= 17] {
    text-name: "[name]";
    text-size: 9;
    text-fill: @transportation-text;
    text-dy: 9;
    text-face-name: @book-fonts;
    text-halo-radius: 1;
    text-wrap-width: @standard-wrap-width;
    text-placement: interior;
    [access != ''][access != 'public'][access != 'yes'] {
      text-fill: #66ccaf;
    }
  }

  [feature = 'amenity_police'][zoom >= 17] {
    text-name: "[name]";
    text-size: 10;
    text-fill: #734a08;
    text-dy: 11;
    text-face-name: @book-fonts;
    text-halo-radius: 1;
    text-wrap-width: @standard-wrap-width;
    text-placement: interior;
  }

  [feature = 'amenity_fire_station'][zoom >= 17] {
    text-name: "[name]";
    text-size: 10;
    text-fill: #734a08;
    text-dy: 11;
    text-face-name: @book-fonts;
    text-halo-radius: 1;
    text-wrap-width: @standard-wrap-width;
    text-placement: interior;
  }

  [feature = 'amenity_place_of_worship'][zoom >= 17] {
    text-name: "[name]";
    text-size: 10;
    text-fill: #000033;
    text-dy: 12;
    text-face-name: @book-fonts;
    text-halo-radius: 1;
    text-wrap-width: @standard-wrap-width;
    text-placement: interior;
  }

  [feature = 'natural_wood'][is_building = 'no'] {
    [zoom >= 8][way_pixels > 3000],
    [zoom >= 17] {
      text-name: "[name]";
      text-size: @landcover-font-size;
      [way_pixels > 12000] { text-size: @landcover-font-size-big; }
      [way_pixels > 48000] { text-size: @landcover-font-size-bigger; }
      text-fill: darken(@wood, 40%);
      text-face-name: @landcover-face-name;
      text-halo-radius: 1;
      text-halo-fill: rgba(255,255,255,0.6);
      text-wrap-width: @landcover-wrap-width-size;
      [way_pixels > 12000] {text-wrap-width: @landcover-wrap-width-size-big; }
      [way_pixels > 48000] {text-wrap-width: @landcover-wrap-width-size-bigger; }
      text-placement: interior;
    }
  }

  [feature = 'natural_peak'],
  [feature = 'natural_volcano'] {
    [zoom >= 13] {
      text-name: "[name]";
      text-size: 10;
      text-fill: brown;
      text-dy: 7;
      text-face-name: @book-fonts;
      text-halo-radius: 1;
      text-wrap-width: @standard-wrap-width;
      text-placement: interior;
      ele/text-name: "[ele]";
      ele/text-size: 9;
      ele/text-fill: brown;
      ele/text-dy: 6;
      ele/text-face-name: @oblique-fonts;
      ele/text-halo-radius: 1;
      ele/text-placement: interior;
      [name != ''] {
        ele/text-dy: 19;
      }
    }
  }

  [feature = 'natural_saddle'] {
    [zoom >= 15] {
      text-name: "[name]";
      text-size: 10;
      text-fill: brown;
      text-dy: 7;
      text-face-name: @book-fonts;
      text-halo-radius: 1;
      text-wrap-width: @standard-wrap-width;
      text-placement: interior;
      ele/text-name: "[ele]";
      ele/text-size: 9;
      ele/text-fill: brown;
      ele/text-dy: 6;
      ele/text-face-name: @oblique-fonts;
      ele/text-halo-radius: 1;
      ele/text-placement: interior;
      [name != ''] {
        ele/text-dy: 19;
      }
    }
  }

  [feature = 'natural_cave_entrance'][zoom >= 15] {
    text-name: "[name]";
    text-size: 10;
    text-fill: brown;
    text-dy: 11;
    text-face-name: @book-fonts;
    text-halo-radius: 1;
    text-wrap-width: @standard-wrap-width;
    text-placement: interior;
  }

  [feature = 'historic_memorial'][zoom >= 17] {
    text-name: "[name]";
    text-size: 9;
    text-fill: brown;
    text-dy: 13;
    text-face-name: @book-fonts;
    text-halo-radius: 1;
    text-wrap-width: @standard-wrap-width;
    text-placement: interior;
  }

  [feature = 'historic_archaeological_site'][zoom >= 17] {
    text-name: "[name]";
    text-size: 9;
    text-fill: brown;
    text-dy: 15;
    text-face-name: @book-fonts;
    text-halo-radius: 1;
    text-wrap-width: @standard-wrap-width;
    text-placement: interior;
  }

  [feature = 'natural_water'],
  [feature = 'natural_lake'],
  [feature = 'landuse_reservoir'],
  [feature = 'landuse_basin'] {
    [way_area >= 40000000][zoom >= 10],
    [way_area >= 10000000][zoom >= 11],
    [way_area >= 2400000][zoom >= 12],
    [way_area >= 600000][zoom >= 13],
    [way_area >= 150000][zoom >= 14],
    [way_area >= 80000][zoom >= 15],
    [way_area >= 20000][zoom >= 16],
    [zoom >= 17] {
      text-name: "[name]";
      text-size: 12;
      text-fill: @water-text;
      text-face-name: @oblique-fonts;
      text-halo-radius: 1;
      text-wrap-width: @standard-wrap-width;
      text-placement: interior;
    }
  }

  [feature = 'natural_mud'][is_building = 'no'] {
    [zoom >= 10][way_pixels > 3000],
    [zoom >= 17] {
      text-name: "[name]";
      text-size: @landcover-font-size;
      [way_pixels > 12000] { text-size: @landcover-font-size-big; }
      [way_pixels > 48000] { text-size: @landcover-font-size-bigger; }
      text-fill: @mud-text;
      text-face-name: @landcover-face-name;
      text-halo-radius: 1;
      text-wrap-width: @landcover-wrap-width-size;
      [way_pixels > 12000] {text-wrap-width: @landcover-wrap-width-size-big; }
      [way_pixels > 48000] {text-wrap-width: @landcover-wrap-width-size-bigger; }
      text-placement: interior;
    }
  }

  [feature = 'natural_wetland'][is_building = 'no'],
  [feature = 'natural_marsh'][is_building = 'no'] {
    [zoom >= 10][way_pixels > 3000],
    [zoom >= 17] {
      text-name: "[name]";
      text-size: @landcover-font-size;
      [way_pixels > 12000] { text-size: @landcover-font-size-big; }
      [way_pixels > 48000] { text-size: @landcover-font-size-bigger; }
      text-fill: @wetland-text;
      text-face-name: @landcover-face-name;
      text-halo-radius: 1;
      text-wrap-width: @landcover-wrap-width-size;
      [way_pixels > 12000] {text-wrap-width: @landcover-wrap-width-size-big; }
      [way_pixels > 48000] {text-wrap-width: @landcover-wrap-width-size-bigger; }
      text-placement: interior;
    }
  }

  [feature = 'leisure_swimming_pool'][is_building = 'no'] {
    [zoom >= 14][way_pixels > 3000],
    [zoom >= 17] {
      text-name: "[name]";
      text-size: @landcover-font-size;
      [way_pixels > 12000] { text-size: @landcover-font-size-big; }
      [way_pixels > 48000] { text-size: @landcover-font-size-bigger; }
      text-fill: @marina-text;
      text-face-name: @landcover-face-name;
      text-halo-radius: 1;
      text-wrap-width: @landcover-wrap-width-size;
      [way_pixels > 12000] {text-wrap-width: @landcover-wrap-width-size-big; }
      [way_pixels > 48000] {text-wrap-width: @landcover-wrap-width-size-bigger; }
      text-placement: interior;
    }
  }

  [feature = 'leisure_sports_centre'][is_building = 'no'],
  [feature = 'leisure_stadium'][is_building = 'no'] {
    [zoom >= 10][way_pixels > 3000],
    [zoom >= 17] {
      text-name: "[name]";
      text-size: @landcover-font-size;
      [way_pixels > 12000] { text-size: @landcover-font-size-big; }
      [way_pixels > 48000] { text-size: @landcover-font-size-bigger; }
      text-fill: darken(@stadium, 30%);
      text-face-name: @landcover-face-name;
      text-halo-radius: 1;
      text-halo-fill: rgba(255,255,255,0.6);
      text-wrap-width: @landcover-wrap-width-size;
      [way_pixels > 12000] {text-wrap-width: @landcover-wrap-width-size-big; }
      [way_pixels > 48000] {text-wrap-width: @landcover-wrap-width-size-bigger; }
      text-placement: interior;
    }
  }

  [feature = 'leisure_track'][is_building = 'no'] {
    [zoom >= 10][way_pixels > 3000],
    [zoom >= 17] {
      text-name: "[name]";
      text-size: @landcover-font-size;
      [way_pixels > 12000] { text-size: @landcover-font-size-big; }
      [way_pixels > 48000] { text-size: @landcover-font-size-bigger; }
      text-fill: darken(@track, 40%);
      text-face-name: @landcover-face-name;
      text-halo-radius: 1;
      text-wrap-width: @landcover-wrap-width-size;
      [way_pixels > 12000] {text-wrap-width: @landcover-wrap-width-size-big; }
      [way_pixels > 48000] {text-wrap-width: @landcover-wrap-width-size-bigger; }
      text-placement: interior;
    }
  }

  [feature = 'leisure_pitch'][is_building = 'no'] {
    [zoom >= 10][way_pixels > 3000],
    [zoom >= 17] {
      text-name: "[name]";
      text-size: @landcover-font-size;
      [way_pixels > 12000] { text-size: @landcover-font-size-big; }
      [way_pixels > 48000] { text-size: @landcover-font-size-bigger; }
      text-fill: darken(@pitch, 40%);
      text-face-name: @landcover-face-name;
      text-halo-radius: 1;
      text-halo-fill: rgba(255,255,255,0.6);
      text-wrap-width: @landcover-wrap-width-size;
      [way_pixels > 12000] {text-wrap-width: @landcover-wrap-width-size-big; }
      [way_pixels > 48000] {text-wrap-width: @landcover-wrap-width-size-bigger; }
      text-placement: interior;
    }
  }

  [feature = 'leisure_playground'] {
    [way_area >= 150000][zoom >= 14],
    [way_area >= 80000][zoom >= 15],
    [way_area >= 20000][zoom >= 16],
    [zoom >= 17] {
      text-name: "[name]";
      text-size: 11;
      text-dy: 13;
      text-fill: darken(@park, 60%);
      text-face-name: @book-fonts;
      text-halo-radius: 1;
      text-halo-fill: rgba(255,255,255,0.6);
      text-wrap-width: @standard-wrap-width;
      text-placement: interior;
    }
  }

  [feature = 'leisure_park'][is_building = 'no'],
  [feature = 'leisure_recreation_ground'][is_building = 'no'],
  [feature = 'landuse_recreation_ground'][is_building = 'no'],
  [feature = 'landuse_conservation'][is_building = 'no'],
  [feature = 'landuse_village_green'][is_building = 'no'],
  [feature = 'leisure_common'][is_building = 'no'],
  [feature = 'leisure_garden'][is_building = 'no'],
  [feature = 'leisure_golf_course'][is_building = 'no'] {
    [zoom >= 10][way_pixels > 3000],
    [zoom >= 17] {
      text-name: "[name]";
      text-size: @landcover-font-size;
      [way_pixels > 12000] { text-size: @landcover-font-size-big; }
      [way_pixels > 48000] { text-size: @landcover-font-size-bigger; }
      text-fill: darken(@park, 60%);
      text-face-name: @landcover-face-name;
      text-halo-radius: 1;
      text-halo-fill: rgba(255,255,255,0.6);
      text-wrap-width: @landcover-wrap-width-size;
      [way_pixels > 12000] {text-wrap-width: @landcover-wrap-width-size-big; }
      [way_pixels > 48000] {text-wrap-width: @landcover-wrap-width-size-bigger; }
      text-placement: interior;
    }
  }

  [feature = 'boundary_national_park'][is_building = 'no'],
  [feature = 'leisure_nature_reserve'][is_building = 'no'] {
    [zoom >= 8][way_pixels > 3000],
    [zoom >= 17] {
      text-name: "[name]";
      text-size: @landcover-font-size;
      [way_pixels > 12000] { text-size: @landcover-font-size-big; }
      [way_pixels > 48000] { text-size: @landcover-font-size-bigger; }
      text-fill: darken(@park, 70%);
      text-face-name: @landcover-face-name;
      text-halo-radius: 1;
      text-halo-fill: rgba(255,255,255,0.6);
      text-wrap-width: @landcover-wrap-width-size;
      [way_pixels > 12000] {text-wrap-width: @landcover-wrap-width-size-big; }
      [way_pixels > 48000] {text-wrap-width: @landcover-wrap-width-size-bigger; }
      text-placement: interior;
    }
  }

  [feature = 'landuse_quarry'][is_building = 'no'] {
    [zoom >= 10][way_pixels > 3000],
    [zoom >= 17] {
      text-name: "[name]";
      text-size: @landcover-font-size;
      [way_pixels > 12000] { text-size: @landcover-font-size-big; }
      [way_pixels > 48000] { text-size: @landcover-font-size-bigger; }
      text-fill: darken(@quarry, 60%);
      text-face-name: @landcover-face-name;
      text-halo-radius: 1;
      text-halo-fill: rgba(255,255,255,0.6);
      text-wrap-width: @landcover-wrap-width-size;
      [way_pixels > 12000] {text-wrap-width: @landcover-wrap-width-size-big; }
      [way_pixels > 48000] {text-wrap-width: @landcover-wrap-width-size-bigger; }
      text-placement: interior;
    }
  }

  [feature = 'landuse_vineyard'][is_building = 'no'],
  [feature = 'landuse_orchard'][is_building = 'no'] {
    [zoom >= 10][way_pixels > 3000],
    [zoom >= 17] {
      text-name: "[name]";
      text-size: @landcover-font-size;
      [way_pixels > 12000] { text-size: @landcover-font-size-big; }
      [way_pixels > 48000] { text-size: @landcover-font-size-bigger; }
      text-fill: darken(@vineyard, 50%);
      text-face-name: @landcover-face-name;
      text-halo-radius: 1.5; /* extra halo needed to overpower the vineyard polygon pattern */
      text-halo-fill: rgba(255,255,255,0.6);
      text-wrap-width: @landcover-wrap-width-size;
      [way_pixels > 12000] {text-wrap-width: @landcover-wrap-width-size-big; }
      [way_pixels > 48000] {text-wrap-width: @landcover-wrap-width-size-bigger; }
      text-placement: interior;
    }
  }

  [feature = 'landuse_cemetery'][is_building = 'no'],
  [feature = 'amenity_grave_yard'][is_building = 'no'] {
    [zoom >= 10][way_pixels > 3000],
    [zoom >= 17] {
      text-name: "[name]";
      text-size: @landcover-font-size;
      [way_pixels > 12000] { text-size: @landcover-font-size-big; }
      [way_pixels > 48000] { text-size: @landcover-font-size-bigger; }
      text-fill: darken(@cemetery, 50%);
      text-face-name: @landcover-face-name;
      text-halo-radius: 1.5; /* extra halo needed to overpower the cemetery polygon pattern */
      text-halo-fill: rgba(255,255,255,0.6);
      text-wrap-width: @landcover-wrap-width-size;
      [way_pixels > 12000] {text-wrap-width: @landcover-wrap-width-size-big; }
      [way_pixels > 48000] {text-wrap-width: @landcover-wrap-width-size-bigger; }
      text-placement: interior;
    }
  }

  [feature = 'landuse_residential'][is_building = 'no'] {
    [zoom >= 10][way_pixels > 3000],
    [zoom >= 17] {
      text-name: "[name]";
      text-size: @landcover-font-size;
      [way_pixels > 12000] { text-size: @landcover-font-size-big; }
      [way_pixels > 48000] { text-size: @landcover-font-size-bigger; }
      text-fill: darken(@residential, 50%);
      text-face-name: @landcover-face-name;
      text-halo-radius: 1;
      text-halo-fill: rgba(255,255,255,0.6);
      text-wrap-width: @landcover-wrap-width-size;
      [way_pixels > 12000] {text-wrap-width: @landcover-wrap-width-size-big; }
      [way_pixels > 48000] {text-wrap-width: @landcover-wrap-width-size-bigger; }
      text-placement: interior;
    }
  }

  [feature = 'landuse_garages'][is_building = 'no'] {
    [zoom >= 13][way_pixels > 3000],
    [zoom >= 17] {
      text-name: "[name]";
      text-size: @landcover-font-size;
      [way_pixels > 12000] { text-size: @landcover-font-size-big; }
      [way_pixels > 48000] { text-size: @landcover-font-size-bigger; }
      text-fill: darken(@garages, 50%);
      text-face-name: @landcover-face-name;
      text-halo-radius: 1;
      text-halo-fill: rgba(255,255,255,0.6);
      text-wrap-width: @landcover-wrap-width-size;
      [way_pixels > 12000] {text-wrap-width: @landcover-wrap-width-size-big; }
      [way_pixels > 48000] {text-wrap-width: @landcover-wrap-width-size-bigger; }
      text-placement: interior;
    }
  }

  [feature = 'landuse_field'][is_building = 'no'] {
    [zoom >= 10][way_pixels > 3000],
    [zoom >= 17] {
      text-name: "[name]";
      text-size: @landcover-font-size;
      [way_pixels > 12000] { text-size: @landcover-font-size-big; }
      [way_pixels > 48000] { text-size: @landcover-font-size-bigger; }
      text-fill: darken(@field, 50%);
      text-face-name: @landcover-face-name;
      text-halo-radius: 1;
      text-halo-fill: rgba(255,255,255,0.6);
      text-wrap-width: @landcover-wrap-width-size;
      [way_pixels > 12000] {text-wrap-width: @landcover-wrap-width-size-big; }
      [way_pixels > 48000] {text-wrap-width: @landcover-wrap-width-size-bigger; }
      text-placement: interior;
    }
  }

  [feature = 'landuse_meadow'][is_building = 'no'],
  [feature = 'landuse_grass'][is_building = 'no'] {
    [zoom >= 10][way_pixels > 3000],
    [zoom >= 17] {
      text-name: "[name]";
      text-size: @landcover-font-size;
      [way_pixels > 12000] { text-size: @landcover-font-size-big; }
      [way_pixels > 48000] { text-size: @landcover-font-size-bigger; }
      text-fill: darken(@grass, 50%);
      text-face-name: @landcover-face-name;
      text-halo-radius: 1;
      text-halo-fill: rgba(255,255,255,0.6);
      text-wrap-width: @landcover-wrap-width-size;
      [way_pixels > 12000] {text-wrap-width: @landcover-wrap-width-size-big; }
      [way_pixels > 48000] {text-wrap-width: @landcover-wrap-width-size-bigger; }
      text-placement: interior;
    }
  }

  [feature = 'landuse_allotments'][is_building = 'no'] {
    [zoom >= 10][way_pixels > 3000],
    [zoom >= 17] {
      text-name: "[name]";
      text-size: @landcover-font-size;
      [way_pixels > 12000] { text-size: @landcover-font-size-big; }
      [way_pixels > 48000] { text-size: @landcover-font-size-bigger; }
      text-fill: darken(@allotments, 50%);
      text-face-name: @landcover-face-name;
      text-halo-radius: 1;
      text-halo-fill: rgba(255,255,255,0.6);
      text-wrap-width: @landcover-wrap-width-size;
      [way_pixels > 12000] {text-wrap-width: @landcover-wrap-width-size-big; }
      [way_pixels > 48000] {text-wrap-width: @landcover-wrap-width-size-bigger; }
      text-placement: interior;
    }
  }

  [feature = 'landuse_forest'][is_building = 'no'] {
    [zoom >= 8][way_pixels > 3000],
    [zoom >= 17] {
      text-name: "[name]";
      text-size: @landcover-font-size;
      [way_pixels > 12000] { text-size: @landcover-font-size-big; }
      [way_pixels > 48000] { text-size: @landcover-font-size-bigger; }
      text-fill: darken(@forest, 30%);
      text-face-name: @landcover-face-name;
      text-halo-radius: 1;
      text-halo-fill: rgba(255,255,255,0.6);
      text-wrap-width: @landcover-wrap-width-size;
      [way_pixels > 12000] {text-wrap-width: @landcover-wrap-width-size-big; }
      [way_pixels > 48000] {text-wrap-width: @landcover-wrap-width-size-bigger; }
      text-placement: interior;
    }
  }

  [feature = 'landuse_farmyard'][is_building = 'no'] {
    [zoom >= 10][way_pixels > 3000],
    [zoom >= 17] {
      text-name: "[name]";
      text-size: @landcover-font-size;
      [way_pixels > 12000] { text-size: @landcover-font-size-big; }
      [way_pixels > 48000] { text-size: @landcover-font-size-bigger; }
      text-fill: darken(@farmyard, 50%);
      text-face-name: @landcover-face-name;
      text-halo-radius: 1;
      text-halo-fill: rgba(255,255,255,0.6);
      text-wrap-width: @landcover-wrap-width-size;
      [way_pixels > 12000] {text-wrap-width: @landcover-wrap-width-size-big; }
      [way_pixels > 48000] {text-wrap-width: @landcover-wrap-width-size-bigger; }
      text-placement: interior;
    }
  }
  [feature = 'landuse_farm'][is_building = 'no'],
  [feature = 'landuse_farmland'][is_building = 'no'] {
    [zoom >= 10][way_pixels > 3000],
    [zoom >= 17] {
      text-name: "[name]";
      text-size: @landcover-font-size;
      [way_pixels > 12000] { text-size: @landcover-font-size-big; }
      [way_pixels > 48000] { text-size: @landcover-font-size-bigger; }
      text-fill: darken(@farmland, 50%);
      text-face-name: @landcover-face-name;
      text-halo-radius: 1;
      text-halo-fill: rgba(255,255,255,0.6);
      text-wrap-width: @landcover-wrap-width-size;
      [way_pixels > 12000] {text-wrap-width: @landcover-wrap-width-size-big; }
      [way_pixels > 48000] {text-wrap-width: @landcover-wrap-width-size-bigger; }
      text-placement: interior;
    }
  }

  [feature = 'shop_mall'],
  [feature = 'landuse_retail'][is_building = 'no'] {
    [zoom >= 10][way_pixels > 3000],
    [zoom >= 17] {
      text-name: "[name]";
      text-size: @landcover-font-size;
      [way_pixels > 12000] { text-size: @landcover-font-size-big; }
      [way_pixels > 48000] { text-size: @landcover-font-size-bigger; }
      text-fill: darken(@retail, 50%);
      text-face-name: @landcover-face-name;
      text-halo-radius: 1;
      text-halo-fill: rgba(255,255,255,0.6);
      text-wrap-width: @landcover-wrap-width-size;
      [way_pixels > 12000] {text-wrap-width: @landcover-wrap-width-size-big; }
      [way_pixels > 48000] {text-wrap-width: @landcover-wrap-width-size-bigger; }
      text-placement: interior;
    }
  }

  [feature = 'landuse_industrial'][is_building = 'no'],
  [feature = 'landuse_railway'][is_building = 'no'] {
    [zoom >= 10][way_pixels > 3000],
    [zoom >= 17] {
      text-name: "[name]";
      text-size: @landcover-font-size;
      [way_pixels > 12000] { text-size: @landcover-font-size-big; }
      [way_pixels > 48000] { text-size: @landcover-font-size-bigger; }
      text-fill: darken(@industrial, 60%);
      text-face-name: @landcover-face-name;
      text-halo-radius: 1;
      text-halo-fill: rgba(255,255,255,0.6);
      text-wrap-width: @landcover-wrap-width-size;
      [way_pixels > 12000] {text-wrap-width: @landcover-wrap-width-size-big; }
      [way_pixels > 48000] {text-wrap-width: @landcover-wrap-width-size-bigger; }
      text-placement: interior;
    }
  }

  [feature = 'landuse_commercial'][is_building = 'no'] {
    [zoom >= 10][way_pixels > 3000],
    [zoom >= 17] {
      text-name: "[name]";
      text-size: @landcover-font-size;
      [way_pixels > 12000] { text-size: @landcover-font-size-big; }
      [way_pixels > 48000] { text-size: @landcover-font-size-bigger; }
      text-fill: darken(@commercial, 60%);
      text-face-name: @landcover-face-name;
      text-halo-radius: 1;
      text-halo-fill: rgba(255,255,255,0.6);
      text-wrap-width: @landcover-wrap-width-size;
      [way_pixels > 12000] {text-wrap-width: @landcover-wrap-width-size-big; }
      [way_pixels > 48000] {text-wrap-width: @landcover-wrap-width-size-bigger; }
      text-placement: interior;
    }
  }

  [feature = 'landuse_brownfield'][is_building = 'no'],
  [feature = 'landuse_landfill'][is_building = 'no'],
  [feature = 'landuse_construction'][is_building = 'no'] {
    [zoom >= 10][way_pixels > 3000],
    [zoom >= 17] {
      text-name: "[name]";
      text-size: @landcover-font-size;
      [way_pixels > 12000] { text-size: @landcover-font-size-big; }
      [way_pixels > 48000] { text-size: @landcover-font-size-bigger; }
      text-fill: darken(@construction, 50%);
      text-face-name: @landcover-face-name;
      text-halo-radius: 1;
      text-halo-fill: rgba(255,255,255,0.6);
      text-wrap-width: @landcover-wrap-width-size;
      [way_pixels > 12000] {text-wrap-width: @landcover-wrap-width-size-big; }
      [way_pixels > 48000] {text-wrap-width: @landcover-wrap-width-size-bigger; }
      text-placement: interior;
    }
  }

  [feature = 'natural_bay'][zoom >= 14] {
    text-name: "[name]";
    text-size: 10;
    text-fill: #6699cc;
    text-face-name: @book-fonts;
    text-halo-radius: 1;
    text-wrap-width: @standard-wrap-width;
    text-placement: interior;
  }

  [feature = 'natural_spring'][zoom >= 16] {
    text-name: "[name]";
    text-size: 10;
    text-fill: #6699cc;
    text-face-name: @book-fonts;
    text-halo-radius: 1;
    text-wrap-width: @standard-wrap-width;
    text-placement: interior;
    text-dy: 6;
  }

  [feature = 'tourism_alpine_hut'][zoom >= 15] {
    text-name: "[name]";
    text-size: 9;
    text-fill: #6699cc;
    text-dy: 11;
    text-face-name: @book-fonts;
    text-halo-radius: 1;
    text-wrap-width: @standard-wrap-width;
    text-placement: interior;
    [zoom >= 16] {
      ele/text-name: "[ele]";
      ele/text-size: 8;
      ele/text-fill: #6699cc;
      ele/text-dy: 23;
      ele/text-face-name: @oblique-fonts;
      ele/text-halo-radius: 1;
      ele/text-placement: interior;
    }
  }

  [feature = 'amenity_shelter'][zoom >= 17] {
    text-name: "[name]";
    text-size: 9;
    text-fill: #6699cc;
    text-dy: 11;
    text-face-name: @book-fonts;
    text-halo-radius: 1;
    text-wrap-width: @standard-wrap-width;
    text-placement: interior;
    ele/text-name: "[ele]";
    ele/text-size: 8;
    ele/text-fill: #6699cc;
    ele/text-dy: 23;
    ele/text-face-name: @oblique-fonts;
    ele/text-halo-radius: 1;
    ele/text-placement: interior;
  }

  [feature = 'amenity_bank'][zoom >= 17] {
    text-name: "[name]";
    text-size: 9;
    text-fill: black;
    text-dy: 12;
    text-halo-radius: 1;
    text-wrap-width: @standard-wrap-width;
    text-placement: interior;
    text-face-name: @book-fonts;
  }

  [feature = 'tourism_hotel'],
  [feature = 'tourism_motel'],
  [feature = 'tourism_hostel'],
  [feature = 'tourism_chalet'] {
    [zoom >= 17] {
      text-name: "[name]";
      text-size: 10;
      text-fill: #0066ff;
      text-dy: 13;
      text-face-name: @book-fonts;
      text-halo-radius: 1;
      text-wrap-width: @standard-wrap-width;
      text-placement: interior;
      [feature = 'tourism_chalet'] {
        text-dy: 11;
      }
    }
  }

  [feature = 'amenity_embassy'][zoom >= 17] {
    text-name: "[name]";
    text-size: 9;
    text-fill: #0066ff;
    text-dy: 9;
    text-face-name: @book-fonts;
    text-halo-radius: 1;
    text-wrap-width: @standard-wrap-width;
    text-placement: interior;
  }

  [feature = 'tourism_guest_house'][zoom >= 17] {
    text-name: "[name]";
    text-size: 8;
    text-fill: #0066ff;
    text-dy: 10;
    text-face-name: @book-fonts;
    text-halo-radius: 1;
    text-wrap-width: @standard-wrap-width;
    text-placement: interior;
  }

  [feature = 'highway_bus_stop'],
  [feature = 'amenity_fuel'],
  [feature = 'amenity_bus_station'] {
    [zoom >= 17] {
      text-name: "[name]";
      text-size: 9;
      text-fill: @transportation-text;
      text-dy: 11;
      text-face-name: @book-fonts;
      text-halo-radius: 1;
      text-wrap-width: @standard-wrap-width;
      text-placement: interior;
      [feature = 'highway_bus_stop'] {
        text-dy: 9;
      }
    }
  }

  [feature = 'tourism_camp_site'][zoom >= 17] {
    text-name: "[name]";
    text-size: 10;
    text-fill: #0066ff;
    text-dy: 15;
    text-face-name: @book-fonts;
    text-halo-radius: 1;
    text-wrap-width: @standard-wrap-width;
    text-placement: interior;
  }

  [feature = 'tourism_caravan_site'][is_building = 'no'] {
    [zoom >= 10][way_pixels > 3000],
    [zoom >= 17] {
      text-name: "[name]";
      text-size: @landcover-font-size;
      [way_pixels > 12000] { text-size: @landcover-font-size-big; }
      [way_pixels > 48000] { text-size: @landcover-font-size-bigger; }
      text-fill: darken(@campsite, 50%);
      text-dy: 15;
      text-face-name: @landcover-face-name;
      text-halo-radius: 1;
      text-halo-fill: rgba(255,255,255,0.6);
      text-wrap-width: @landcover-wrap-width-size;
      [way_pixels > 12000] {text-wrap-width: @landcover-wrap-width-size-big; }
      [way_pixels > 48000] {text-wrap-width: @landcover-wrap-width-size-bigger; }
      text-placement: interior;
    }
  }

  [feature = 'waterway_lock'][zoom >= 15] {
    text-name: "[name]";
    text-size: 9;
    text-dy: 10;
    text-fill: #0066ff;
    text-face-name: @book-fonts;
    text-halo-radius: 1;
    text-wrap-width: @standard-wrap-width;
    text-placement: interior;
  }

  [feature = 'leisure_marina'][zoom >= 15] {
    text-name: "[name]";
    text-size: 8;
    text-fill: @marina-text;
    text-face-name: @book-fonts;
    text-halo-radius: 1;
    text-wrap-width: @standard-wrap-width;
    text-placement: interior;
    [zoom >= 17] {
      text-size: 10;
    }
  }

  [feature = 'tourism_theme_park'][is_building = 'no'] {
    [zoom >= 13][way_pixels > 3000],
    [zoom >= 17] {
      text-name: "[name]";
      text-size: @landcover-font-size;
      [way_pixels > 12000] { text-size: @landcover-font-size-big; }
      [way_pixels > 48000] { text-size: @landcover-font-size-bigger; }
      text-fill: @theme_park;
      text-face-name: @bold-fonts; /*rendered bold to improve visibility since theme parks tend to have crowded backgrounds*/
      text-halo-radius: 1;
      text-halo-fill: rgba(255,255,255,0.6);
      text-wrap-width: @landcover-wrap-width-size;
      [way_pixels > 12000] {text-wrap-width: @landcover-wrap-width-size-big; }
      [way_pixels > 48000] {text-wrap-width: @landcover-wrap-width-size-bigger; }
      text-placement: interior;
    }
  }

  [feature = 'tourism_museum'][zoom >= 17] {
    text-name: "[name]";
    text-size: 10;
    text-dy: 11;
    text-fill: #734a08;
    text-face-name: @book-fonts;
    text-halo-radius: 1;
    text-wrap-width: @standard-wrap-width;
    text-placement: interior;
  }

  [feature = 'amenity_prison'][zoom >= 17] {
    text-name: "[name]";
    text-size: 10;
    text-fill: #734a08;
    text-dy: 16;
    text-face-name: @book-fonts;
    text-halo-radius: 1;
    text-wrap-width: @standard-wrap-width;
    text-placement: interior;
  }

  [feature = 'tourism_attraction'][is_building = 'no'] {
    [zoom >= 10][way_pixels > 3000],
    [zoom >= 17] {
      text-name: "[name]";
      text-size: @landcover-font-size;
      [way_pixels > 12000] { text-size: @landcover-font-size-big; }
      [way_pixels > 48000] { text-size: @landcover-font-size-bigger; }
      text-fill: #660033;
      text-face-name: @book-fonts;
      text-halo-radius: 2;
      text-wrap-width: @landcover-wrap-width-size;
      [way_pixels > 12000] {text-wrap-width: @landcover-wrap-width-size-big; }
      [way_pixels > 48000] {text-wrap-width: @landcover-wrap-width-size-bigger; }
      text-placement: interior;
    }
  }

  [feature = 'amenity_university'][is_building = 'no'] {
    [zoom >= 10][way_pixels > 3000],
    [zoom >= 17] {
      text-name: "[name]";
      text-size: @landcover-font-size;
      [way_pixels > 12000] { text-size: @landcover-font-size-big; }
      [way_pixels > 48000] { text-size: @landcover-font-size-bigger; }
      text-fill: darken(@school, 70%);
      text-face-name: @landcover-face-name;
      text-halo-radius: 1;
      text-halo-fill: rgba(255,255,255,0.6);
      text-wrap-width: @landcover-wrap-width-size;
      [way_pixels > 12000] {text-wrap-width: @landcover-wrap-width-size-big; }
      [way_pixels > 48000] {text-wrap-width: @landcover-wrap-width-size-bigger; }
      text-placement: interior;
    }
  }

  [feature = 'amenity_school'][is_building = 'no'],
  [feature = 'amenity_college'][is_building = 'no'] {
    [zoom >= 10][way_pixels > 3000],
    [zoom >= 17] {
      text-name: "[name]";
      text-size: @landcover-font-size;
      [way_pixels > 12000] { text-size: @landcover-font-size-big; }
      [way_pixels > 48000] { text-size: @landcover-font-size-bigger; }
      text-fill: darken(@school, 70%);
      text-face-name: @landcover-face-name;
      text-halo-radius: 1;
      text-halo-fill: rgba(255,255,255,0.6);
      text-wrap-width: @landcover-wrap-width-size;
      [way_pixels > 12000] {text-wrap-width: @landcover-wrap-width-size-big; }
      [way_pixels > 48000] {text-wrap-width: @landcover-wrap-width-size-bigger; }
      text-placement: interior;
    }
  }

  [feature = 'amenity_kindergarten'][is_building = 'no'] {
    [zoom >= 10][way_pixels > 3000],
    [zoom >= 17] {
      text-name: "[name]";
      text-size: @landcover-font-size;
      [way_pixels > 12000] { text-size: @landcover-font-size-big; }
      [way_pixels > 48000] { text-size: @landcover-font-size-bigger; }
      text-fill: darken(@school, 65%);
      text-face-name: @landcover-face-name;
      text-halo-radius: 1;
      text-halo-fill: rgba(255,255,255,0.6);
      text-wrap-width: @landcover-wrap-width-size;
      [way_pixels > 12000] {text-wrap-width: @landcover-wrap-width-size-big; }
      [way_pixels > 48000] {text-wrap-width: @landcover-wrap-width-size-bigger; }
      text-placement: interior;
    }
  }

  [feature = 'man_made_lighthouse'][zoom >= 15] {
    text-name: "[name]";
    text-size: 9;
    text-fill: #000033;
    text-dy: 16;
    text-face-name: @book-fonts;
    text-halo-radius: 2;
    text-wrap-width: @standard-wrap-width;
    text-placement: interior;
  }

  [feature = 'man_made_windmill'][zoom >= 17] {
    text-name: "[name]";
    text-size: 9;
    text-fill: #734a08;
    text-dy: 12;
    text-face-name: @book-fonts;
    text-halo-radius: 1;
    text-wrap-width: @standard-wrap-width;
    text-placement: interior;
  }

  [feature = 'amenity_hospital'][zoom >= 16] {
    text-name: "[name]";
    text-fill: #da0092;
    text-size: 8;
    text-dy: 10;
    text-face-name: @book-fonts;
    text-halo-radius: 2;
    text-wrap-width: @standard-wrap-width;
    text-placement: interior;
  }

  [feature = 'amenity_pharmacy'][zoom >= 17] {
    text-name: "[name]";
    text-size: 8;
    text-dy: 10;
    text-fill: #da0092;
    text-face-name: @book-fonts;
    text-halo-radius: 1;
    text-wrap-width: @standard-wrap-width;
    text-placement: interior;
  }

  [feature = 'shop_bakery'],
  [feature = 'shop_clothes'],
  [feature = 'shop_fashion'],
  [feature = 'shop_convenience'],
  [feature = 'shop_doityourself'],
  [feature = 'shop_hairdresser'],
  [feature = 'shop_butcher'],
  [feature = 'shop_car'],
  [feature = 'shop_car_repair'],
  [feature = 'shop_bicycle'],
  [feature = 'shop_florist'],
  [feature = 'shop_other']{
    [zoom >= 17] {
      text-name: "[name]";
      text-size: 10;
      text-dy: 12;
      text-fill: #939;
      text-face-name: @book-fonts;
      text-halo-radius: 1.5;
      text-halo-fill: rgba(255, 255, 255, 0.8);
      text-wrap-width: @standard-wrap-width;
      text-placement: interior;
    }
  }

  [feature = 'shop_supermarket'],
  [feature = 'shop_department_store'] {
    [zoom >= 16] {
      text-name: "[name]";
      text-size: 10;
      text-dy: 12;
      text-fill: #939;
      text-face-name: @book-fonts;
      text-halo-radius: 1.5;
      text-halo-fill: rgba(255, 255, 255, 0.8);
      text-wrap-width: @standard-wrap-width;
      text-placement: interior;
    }
  }

  [feature = 'military_danger_area'][is_building = 'no'] {
    [zoom >= 9][way_pixels > 3000],
    [zoom >= 17] {
      text-name: "[name]";
      text-size: @landcover-font-size;
      [way_pixels > 12000] { text-size: @landcover-font-size-big; }
      [way_pixels > 48000] { text-size: @landcover-font-size-bigger; }
      text-fill: darken(@danger_area, 40%);
      text-face-name: @bold-fonts;
      text-halo-radius: 1;
      text-halo-fill: rgba(255,255,255,0.6);
      text-wrap-width: @landcover-wrap-width-size;
      [way_pixels > 12000] {text-wrap-width: @landcover-wrap-width-size-big; }
      [way_pixels > 48000] {text-wrap-width: @landcover-wrap-width-size-bigger; }
      text-placement: interior;
    }
  }

  [feature = 'landuse_military'][is_building = 'no'] {
    [zoom >= 10][way_pixels > 3000],
    [zoom >= 17] {
      text-name: "[name]";
      text-size: @landcover-font-size;
      [way_pixels > 12000] { text-size: @landcover-font-size-big; }
      [way_pixels > 48000] { text-size: @landcover-font-size-bigger; }
      text-fill: darken(@military, 40%);
      text-face-name: @landcover-face-name;
      text-halo-radius: 1;
      text-halo-fill: rgba(255,255,255,0.6);
      text-wrap-width: @landcover-wrap-width-size;
      [way_pixels > 12000] {text-wrap-width: @landcover-wrap-width-size-big; }
      [way_pixels > 48000] {text-wrap-width: @landcover-wrap-width-size-bigger; }
      text-placement: interior;
    }
  }

  [feature = 'aeroway_gate'][zoom >= 17] {
    text-name: "[ref]";
    text-size: 10;
    text-fill: #aa66cc;
    text-face-name: @book-fonts;
    text-halo-radius: 1;
    text-wrap-width: @standard-wrap-width;
    text-placement: interior;
  }

  [feature = 'military_barracks'][is_building = 'no'] {
    [zoom >= 10][way_pixels > 3000],
    [zoom >= 17] {
      text-name: "[name]";
      text-size: @landcover-font-size;
      [way_pixels > 12000] { text-size: @landcover-font-size-big; }
      [way_pixels > 48000] { text-size: @landcover-font-size-bigger; }
      text-fill: darken(@barracks, 40%);
      text-face-name: @landcover-face-name;
      text-halo-radius: 1;
      text-halo-fill: rgba(255,255,255,0.6);
      text-wrap-width: @landcover-wrap-width-size;
      [way_pixels > 12000] {text-wrap-width: @landcover-wrap-width-size-big; }
      [way_pixels > 48000] {text-wrap-width: @landcover-wrap-width-size-bigger; }
      text-placement: interior;
    }
  }

  [feature = 'tourism_zoo'][is_building = 'no'] {
    [zoom >= 10][way_pixels > 3000],
    [zoom >= 17] {
      text-name: "[name]";
      text-size: @landcover-font-size;
      [way_pixels > 12000] { text-size: @landcover-font-size-big; }
      [way_pixels > 48000] { text-size: @landcover-font-size-bigger; }
      text-fill: darken(@zoo, 40%);
      text-face-name: @landcover-face-name;
      text-halo-radius: 1;
      text-halo-fill: rgba(255,255,255,0.6);
      text-wrap-width: @landcover-wrap-width-size;
      [way_pixels > 12000] {text-wrap-width: @landcover-wrap-width-size-big; }
      [way_pixels > 48000] {text-wrap-width: @landcover-wrap-width-size-bigger; }
      text-placement: interior;
    }
  }

  [feature = 'power_station'][is_building = 'no'][zoom >= 10],
  [feature = 'power_generator'][is_building = 'no'][zoom >= 10],
  [feature = 'power_sub_station'][is_building = 'no'][zoom >= 13],
  [feature = 'power_substation'][is_building = 'no'][zoom >= 13]{
    [way_pixels > 3000],
    [zoom >= 17] {
      text-name: "[name]";
      text-size: @landcover-font-size;
      [way_pixels > 12000] { text-size: @landcover-font-size-big; }
      [way_pixels > 48000] { text-size: @landcover-font-size-bigger; }
      text-fill: darken(@power, 40%);
      text-face-name: @landcover-face-name;
      text-halo-radius: 1;
      text-halo-fill: rgba(255,255,255,0.6);
      text-wrap-width: @landcover-wrap-width-size;
      [way_pixels > 12000] {text-wrap-width: @landcover-wrap-width-size-big; }
      [way_pixels > 48000] {text-wrap-width: @landcover-wrap-width-size-bigger; }
      text-placement: interior;
    }
  }

  [feature = 'natural_desert'][is_building = 'no'] {
    [zoom >= 10][way_pixels > 3000],
    [zoom >= 17] {
      text-name: "[name]";
      text-size: @landcover-font-size;
      [way_pixels > 12000] { text-size: @landcover-font-size-big; }
      [way_pixels > 48000] { text-size: @landcover-font-size-bigger; }
      text-fill: darken(@desert, 40%);
      text-face-name: @landcover-face-name;
      text-halo-radius: 1;
      text-halo-fill: rgba(255,255,255,0.6);
      text-wrap-width: @landcover-wrap-width-size;
      [way_pixels > 12000] {text-wrap-width: @landcover-wrap-width-size-big; }
      [way_pixels > 48000] {text-wrap-width: @landcover-wrap-width-size-bigger; }
      text-placement: interior;
    }
  }

  [feature = 'natural_sand'][is_building = 'no'] {
    [zoom >= 10][way_pixels > 3000],
    [zoom >= 17] {
      text-name: "[name]";
      text-size: @landcover-font-size;
      [way_pixels > 12000] { text-size: @landcover-font-size-big; }
      [way_pixels > 48000] { text-size: @landcover-font-size-bigger; }
      text-fill: darken(@sand, 50%);
      text-face-name: @landcover-face-name;
      text-halo-radius: 1;
      text-halo-fill: rgba(255,255,255,0.6);
      text-wrap-width: @landcover-wrap-width-size;
      [way_pixels > 12000] {text-wrap-width: @landcover-wrap-width-size-big; }
      [way_pixels > 48000] {text-wrap-width: @landcover-wrap-width-size-bigger; }
      text-placement: interior;
    }
  }

  [feature = 'natural_heath'][is_building = 'no'] {
    [zoom >= 10][way_pixels > 3000],
    [zoom >= 17] {
      text-name: "[name]";
      text-size: @landcover-font-size;
      [way_pixels > 12000] { text-size: @landcover-font-size-big; }
      [way_pixels > 48000] { text-size: @landcover-font-size-bigger; }
      text-fill: darken(@heath, 40%);
      text-face-name: @landcover-face-name;
      text-halo-radius: 1;
      text-halo-fill: rgba(255,255,255,0.6);
      text-wrap-width: @landcover-wrap-width-size;
      [way_pixels > 12000] {text-wrap-width: @landcover-wrap-width-size-big; }
      [way_pixels > 48000] {text-wrap-width: @landcover-wrap-width-size-bigger; }
      text-placement: interior;
    }
  }


  [feature = 'natural_grassland'][is_building = 'no'] {
    [zoom >= 10][way_pixels > 3000],
    [zoom >= 17] {
      text-name: "[name]";
      text-size: @landcover-font-size;
      [way_pixels > 12000] { text-size: @landcover-font-size-big; }
      [way_pixels > 48000] { text-size: @landcover-font-size-bigger; }
      text-fill: darken(@grassland, 60%);
      text-face-name: @landcover-face-name;
      text-halo-radius: 1;
      text-halo-fill: rgba(255,255,255,0.6);
      text-wrap-width: @landcover-wrap-width-size;
      [way_pixels > 12000] {text-wrap-width: @landcover-wrap-width-size-big; }
      [way_pixels > 48000] {text-wrap-width: @landcover-wrap-width-size-bigger; }
      text-placement: interior;
    }
  }

  [feature = 'natural_scrub'][is_building = 'no'] {
    [zoom >= 10][way_pixels > 3000],
    [zoom >= 17] {
      text-name: "[name]";
      text-size: @landcover-font-size;
      [way_pixels > 12000] { text-size: @landcover-font-size-big; }
      [way_pixels > 48000] { text-size: @landcover-font-size-bigger; }
      text-fill: darken(@scrub, 60%);
      text-face-name: @landcover-face-name;
      text-halo-radius: 1;
      text-halo-fill: rgba(255,255,255,0.6);
      text-wrap-width: @landcover-wrap-width-size;
      [way_pixels > 12000] {text-wrap-width: @landcover-wrap-width-size-big; }
      [way_pixels > 48000] {text-wrap-width: @landcover-wrap-width-size-bigger; }
      text-placement: interior;
    }
  }

  [feature = 'aeroway_apron'][is_building = 'no'] {
    [zoom >= 10][way_pixels > 3000],
    [zoom >= 17] {
      text-name: "[name]";
      text-size: @landcover-font-size;
      [way_pixels > 12000] { text-size: @landcover-font-size-big; }
      [way_pixels > 48000] { text-size: @landcover-font-size-bigger; }
      text-fill: darken(@apron, 60%);
      text-face-name: @landcover-face-name;
      text-halo-radius: 1;
      text-halo-fill: rgba(255,255,255,0.6);
      text-wrap-width: @landcover-wrap-width-size;
      [way_pixels > 12000] {text-wrap-width: @landcover-wrap-width-size-big; }
      [way_pixels > 48000] {text-wrap-width: @landcover-wrap-width-size-bigger; }
      text-placement: interior;
    }
  }

  [feature = 'natural_beach'][is_building = 'no'] {
    [zoom >= 10][way_pixels > 3000],
    [zoom >= 17] {
      text-name: "[name]";
      text-size: @landcover-font-size;
      [way_pixels > 12000] { text-size: @landcover-font-size-big; }
      [way_pixels > 48000] { text-size: @landcover-font-size-bigger; }
      text-fill: darken(@beach, 60%);
      text-face-name: @landcover-face-name;
      text-halo-radius: 1;
      text-halo-fill: rgba(255,255,255,0.6);
      text-wrap-width: @landcover-wrap-width-size;
      [way_pixels > 12000] {text-wrap-width: @landcover-wrap-width-size-big; }
      [way_pixels > 48000] {text-wrap-width: @landcover-wrap-width-size-bigger; }
      text-placement: interior;
    }
  }

  [feature = 'highway_services'][is_building = 'no'],
  [feature = 'highway_rest_area'][is_building = 'no'] {
    [zoom >= 10][way_pixels > 3000],
    [zoom >= 17] {
      text-name: "[name]";
      text-size: @landcover-font-size;
      [way_pixels > 12000] { text-size: @landcover-font-size-big; }
      [way_pixels > 48000] { text-size: @landcover-font-size-bigger; }
      text-fill: darken(@rest_area, 40%);
      text-face-name: @landcover-face-name;
      text-halo-radius: 1;
      text-halo-fill: rgba(255,255,255,0.6);
      text-wrap-width: @landcover-wrap-width-size;
      [way_pixels > 12000] {text-wrap-width: @landcover-wrap-width-size-big; }
      [way_pixels > 48000] {text-wrap-width: @landcover-wrap-width-size-bigger; }
    }
  }

  [feature = 'natural_glacier'][is_building = 'no'] {
    [zoom >= 10][way_pixels > 3000],
    [zoom >= 17] {
      text-name: "[name]";
      text-size: @landcover-font-size;
      [way_pixels > 12000] { text-size: @landcover-font-size-big; }
      [way_pixels > 48000] { text-size: @landcover-font-size-bigger; }
      text-fill: darken(@glacier, 40%);
      text-face-name: @landcover-face-name;
      text-halo-radius: 1;
      text-halo-fill: rgba(255,255,255,0.6);
      text-wrap-width: @landcover-wrap-width-size;
      [way_pixels > 12000] {text-wrap-width: @landcover-wrap-width-size-big; }
      [way_pixels > 48000] {text-wrap-width: @landcover-wrap-width-size-bigger; }
    }
  }

  [feature = 'aeroway_helipad'][zoom >= 16]::aeroway {
    text-name: "[name]";
    text-size: 8;
    text-fill: @airtransport;
    text-dy: -10;
    text-face-name: @bold-fonts;
    text-halo-radius: 1;
    text-placement: interior;
    text-wrap-width: 30;
  }

  [feature = 'aeroway_aerodrome'][zoom >= 10][zoom < 14]::aeroway {
    text-name: "[name]";
    text-size: 8;
    text-fill: darken(@airtransport, 15%);
    text-dy: -10;
    text-face-name: @oblique-fonts;
    text-halo-radius: 1;
    text-placement: interior;
    text-wrap-width: 30;
  }
}
