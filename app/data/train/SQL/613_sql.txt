CREATE TABLE work_track_map(
    track_id int8,
    work_movement_id int8
);

CREATE TABLE tempo(
    id int4,
    tempo_name varchar(64)
);

CREATE TABLE work_movement (
    id int8,
    work_id int8,
    movement_id int4,
    tempo_id int4,
);

CREATE TABLE works (
    id int8,
    work_name varchar(1024),
    composer_id int4,
    tonality_id int4,
    note_id,
    opus_name varchar(4)
);

-- 作曲家
CREATE TABLE composer (
    id int4,
    composer_name varchar(128)
);

-- 調性
CREATE TABLE tonality (
    id int4,
    tonality_name varchar(32)
);

-- 唱名
CREATE TABLE note (
    id int2,
    note_name,
);

-- 配器, 鋼琴, 小提琴, 樂團

-- 編制, 鋼琴演奏者, 小提琴演奏者, 交響樂團 http://en.wikipedia.org/wiki/Musical_ensemble#Classical_chamber_music

-- 曲式, 奏鳴曲, 交響曲, 協奏曲 http://en.wikipedia.org/wiki/Category:Western_classical_music_styles
CREATE TABLE music_styles (
    id int4,
    music_style_name varchar(128),
);
