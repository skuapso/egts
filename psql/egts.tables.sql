create table egts.navigation(
  id bigint,
  eventtime timestamp with time zone not null,
  latitude real not null,
  longitude real not null,
  parking boolean not null,
  online boolean not null,
  fix bigint not null,
  cs bigint not null,
  valid bigint not null,
  speed real not null,
  course bigint not null,
  odometer bigint not null,
  event bigint not null,
  altitude bigint,
  src_data bigint,
  vdop bigint,
  hdop bigint,
  pdop bigint,
  used bigint,
  navigation_systems egts.navigation_systems,
  state egts.states,
  
  constraint zidx_navigation_pk primary key(id),
  constraint zidx_navigation_fk foreign key(id) references data.packets(id) on delete cascade
);

create table egts.digital_in(
  id bigint,
  sensor bigint not null,
  value boolean not null,

  constraint zidx_digital_in_pk primary key(id, sensor),
  constraint zidx_digital_in_fk foreign key(id) references egts.navigation(id) on delete cascade
);

create table egts.digital_out(
  id bigint,
  sensor bigint not null,
  value boolean not null,

  constraint zidx_digital_out_pk primary key(id, sensor),
  constraint zidx_digital_out_fk foreign key(id) references egts.navigation(id) on delete cascade
);

create table egts.analog(
  id bigint,
  sensor bigint not null,
  value real not null,
  constraint zidx_analog_pk primary key(id, sensor),
  constraint zidx_analog_fk foreign key(id) references egts.navigation(id) on delete cascade
);

create table egts.counter(
  id bigint,
  sensor bigint not null,
  value boolean not null,

  constraint zidx_counters_pk primary key(id, sensor),
  constraint zidx_counters_fk foreign key(id) references egts.navigation(id) on delete cascade
);

create table egts.boolean_sensor(
  id bigint,
  sensor egts.boolean_sensors not null,
  value boolean not null,
  
  constraint zidx_boolean_sensor_pk primary key(id, sensor),
  constraint zidx_boolean_sensor_fk foreign key(id) references egts.navigation(id) on delete cascade
);

create table egts.float_sensor(
  id bigint,
  sensor egts.float_sensors not null,
  value boolean not null,
  
  constraint zidx_float_sensor_pk primary key(id, sensor),
  constraint zidx_float_sensor_fk foreign key(id) references egts.navigation(id) on delete cascade
);

create table egts.loop_in_sensor(
  id bigint,
  sensor bigint not null,
  value boolean not null,
  
  constraint zidx_loop_in_pk primary key(id, sensor),
  constraint zidx_loop_in_fk foreign key(id) references egts.navigation(id) on delete cascade
);

create table egts.lls(
  id bigint,
  sensor bigint not null,
  value boolean not null,
  
  constraint zidx_lls_pk primary key(id, sensor),
  constraint zidx_lls_fk foreign key(id) references egts.navigation(id) on delete cascade
);

create table egts.lls_port(
  id bigint,
  sensor bigint not null,
  value boolean not null,
  
  constraint zidx_lls_port_pk primary key(id, sensor),
  constraint zidx_lls_port_fk foreign key(id) references egts.navigation(id) on delete cascade
);

create table egts.passengers_bin(
  id bigint,
  port bigint not null,
  doors_present bigint not null,
  doors_released bigint not null,
  data bytea not null,

  constraint zidx_passengers_bin_pk primary key(id, port),
  constraint zidx_passengers_bin_fk foreign key(id) references egts.navigation(id) on delete cascade
);

create table egts.passengers_in(
  id bigint,
  sensor bigint not null,
  value bigint not null,

  constraint zidx_passengers_in_pk primary key(id, sensor),
  constraint zidx_passengers_in_fk foreign key(id) references egts.navigation(id) on delete cascade
);

create table egts.passengers_out(
  id bigint,
  sensor bigint not null,
  value bigint not null,

  constraint zidx_passengers_out_pk primary key(id, sensor),
  constraint zidx_passengers_out_fk foreign key(id) references egts.navigation(id) on delete cascade
);
