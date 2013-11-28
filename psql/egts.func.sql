alter type terminals.protocols add value 'egts';

create type egts.navigation_systems as enum(
  'glonass', 'gps', 'galileo', 'compass', 'beidou', 'doris', 'irmss', 'qzss');
create type egts.boolean_sensors as  enum(
  'navigation', 'backup_battery_using', 'internal_battery_using');
create type egts.float_sensors as enum(
  'main_power', 'backup_battery', 'internal_battery');
create type egts.states as enum(
  'passive', 'era', 'active', 'extra_call', 'extra_tracking', 'testing', 'service', 'firmware');
