-- Name: workerpopulationratio; Type: TABLE; Schema: public; Owner: postgres; Tablespace:
CREATE TABLE IF NOT EXISTS workerpopulationratio(
id serial NOT NULL,
 rural_male bigint,
  rural_female bigint,
  rural_person bigint,
  urban_male bigint,
  urban_female bigint,
  urban_person bigint,
  rural_urban_male bigint,
  rural_urban_female bigint,
  rural_urban_person bigint,
  caste text,
  type text,
  year integer,
  CONSTRAINT "WorkerPopulationRatio_pkey" PRIMARY KEY (id)
);

-- Data for Name: workerpopulationratio; Type: TABLE DATA; Schema: public; Owner: postgres

INSERT INTO workerpopulationratio (id, rural_male, rural_female, rural_person, urban_male, urban_female, urban_person, rural_urban_male, rural_urban_female, rural_urban_person, caste, type, year) VALUES (17, 778, 309, 557, 736, 201, 479, 770, 287, 541, 'SC', 'UPSS', 2013);
INSERT INTO workerpopulationratio (id, rural_male, rural_female, rural_person, urban_male, urban_female, urban_person, rural_urban_male, rural_urban_female, rural_urban_person, caste, type, year) VALUES (18, 780, 423, 609, 691, 193, 450, 766, 386, 583, 'ST ', 'UPSS', 2013);
INSERT INTO workerpopulationratio (id, rural_male, rural_female, rural_person, urban_male, urban_female, urban_person, rural_urban_male, rural_urban_female, rural_urban_person, caste, type, year) VALUES (19, 756, 277, 529, 721, 163, 453, 747, 247, 509, 'OBC', 'UPSS', 2013);
INSERT INTO workerpopulationratio (id, rural_male, rural_female, rural_person, urban_male, urban_female, urban_person, rural_urban_male, rural_urban_female, rural_urban_person, caste, type, year) VALUES (20, 746, 226, 500, 679, 133, 419, 721, 190, 469, 'OTHERS', 'UPSS', 2013);
INSERT INTO workerpopulationratio (id, rural_male, rural_female, rural_person, urban_male, urban_female, urban_person, rural_urban_male, rural_urban_female, rural_urban_person, caste, type, year) VALUES (9, 783, 301, 556, 725, 202, 482, 771, 282, 542, 'SC', 'UPS', 2012);
INSERT INTO workerpopulationratio (id, rural_male, rural_female, rural_person, urban_male, urban_female, urban_person, rural_urban_male, rural_urban_female, rural_urban_person, caste, type, year) VALUES (10, 801, 371, 595, 703, 206, 464, 790, 352, 581, 'ST', 'UPS', 2012);
INSERT INTO workerpopulationratio (id, rural_male, rural_female, rural_person, urban_male, urban_female, urban_person, rural_urban_male, rural_urban_female, rural_urban_person, caste, type, year) VALUES (11, 772, 266, 533, 729, 167, 462, 762, 242, 516, 'OBC', 'UPS', 2012);
INSERT INTO workerpopulationratio (id, rural_male, rural_female, rural_person, urban_male, urban_female, urban_person, rural_urban_male, rural_urban_female, rural_urban_person, caste, type, year) VALUES (12, 730, 192, 478, 687, 129, 425, 714, 169, 458, 'OTHERS', 'UPS', 2012);
INSERT INTO workerpopulationratio (id, rural_male, rural_female, rural_person, urban_male, urban_female, urban_person, rural_urban_male, rural_urban_female, rural_urban_person, caste, type, year) VALUES (13, 791, 355, 586, 730, 220, 493, 780, 329, 568, 'SC', 'UPSS', 2012);
INSERT INTO workerpopulationratio (id, rural_male, rural_female, rural_person, urban_male, urban_female, urban_person, rural_urban_male, rural_urban_female, rural_urban_person, caste, type, year) VALUES (14, 813, 480, 654, 712, 238, 484, 802, 453, 635, 'ST', 'UPSS', 2012);
INSERT INTO workerpopulationratio (id, rural_male, rural_female, rural_person, urban_male, urban_female, urban_person, rural_urban_male, rural_urban_female, rural_urban_person, caste, type, year) VALUES (15, 780, 324, 565, 733, 180, 471, 769, 289, 542, 'OBC', 'UPSS', 2012);
INSERT INTO workerpopulationratio (id, rural_male, rural_female, rural_person, urban_male, urban_female, urban_person, rural_urban_male, rural_urban_female, rural_urban_person, caste, type, year) VALUES (16, 741, 242, 507, 691, 137, 433, 723, 204, 479, 'OTHERS', 'UPSS', 2012);
INSERT INTO workerpopulationratio (id, rural_male, rural_female, rural_person, urban_male, urban_female, urban_person, rural_urban_male, rural_urban_female, rural_urban_person, caste, type, year) VALUES (1, 715, 293, 535, 717, 190, 464, 715, 269, 520, 'SC', 'UPS', 2014);
INSERT INTO workerpopulationratio (id, rural_male, rural_female, rural_person, urban_male, urban_female, urban_person, rural_urban_male, rural_urban_female, rural_urban_person, caste, type, year) VALUES (2, 781, 393, 597, 742, 207, 481, 776, 367, 581, 'ST', 'UPS', 2014);
INSERT INTO workerpopulationratio (id, rural_male, rural_female, rural_person, urban_male, urban_female, urban_person, rural_urban_male, rural_urban_female, rural_urban_person, caste, type, year) VALUES (3, 706, 279, 523, 723, 180, 460, 711, 248, 505, 'OBC', 'UPS', 2014);
INSERT INTO workerpopulationratio (id, rural_male, rural_female, rural_person, urban_male, urban_female, urban_person, rural_urban_male, rural_urban_female, rural_urban_person, caste, type, year) VALUES (4, 705, 191, 473, 690, 133, 424, 699, 166, 453, 'OTHERS', 'UPS', 2014);
INSERT INTO workerpopulationratio (id, rural_male, rural_female, rural_person, urban_male, urban_female, urban_person, rural_urban_male, rural_urban_female, rural_urban_person, caste, type, year) VALUES (21, 743, 381, 589, 724, 203, 474, 739, 340, 565, 'SC', 'UPSS', 2014);
INSERT INTO workerpopulationratio (id, rural_male, rural_female, rural_person, urban_male, urban_female, urban_person, rural_urban_male, rural_urban_female, rural_urban_person, caste, type, year) VALUES (22, 817, 516, 674, 749, 237, 500, 807, 476, 650, 'ST', 'UPSS', 2014);
INSERT INTO workerpopulationratio (id, rural_male, rural_female, rural_person, urban_male, urban_female, urban_person, rural_urban_male, rural_urban_female, rural_urban_person, caste, type, year) VALUES (23, 734, 355, 571, 729, 196, 471, 733, 306, 543, 'OBC', 'UPSS', 2014);
INSERT INTO workerpopulationratio (id, rural_male, rural_female, rural_person, urban_male, urban_female, urban_person, rural_urban_male, rural_urban_female, rural_urban_person, caste, type, year) VALUES (24, 725, 247, 510, 694, 142, 430, 713, 203, 477, 'OTHERS', 'UPSS', 2014);
INSERT INTO workerpopulationratio (id, rural_male, rural_female, rural_person, urban_male, urban_female, urban_person, rural_urban_male, rural_urban_female, rural_urban_person, caste, type, year) VALUES (26, 775, 325, 563, 713, 170, 458, 759, 285, 536, 'OVERALL', 'UPSS', 2012);
INSERT INTO workerpopulationratio (id, rural_male, rural_female, rural_person, urban_male, urban_female, urban_person, rural_urban_male, rural_urban_female, rural_urban_person, caste, type, year) VALUES (25, 766, 265, 529, 709, 157, 449, 751, 236, 508, 'OVERALL', 'UPS', 2012);
INSERT INTO workerpopulationratio (id, rural_male, rural_female, rural_person, urban_male, urban_female, urban_person, rural_urban_male, rural_urban_female, rural_urban_person, caste, type, year) VALUES (28, 761, 287, 536, 704, 157, 442, 745, 250, 510, 'OVERALL', 'UPSS', 2013);
INSERT INTO workerpopulationratio (id, rural_male, rural_female, rural_person, urban_male, urban_female, urban_person, rural_urban_male, rural_urban_female, rural_urban_person, caste, type, year) VALUES (29, 716, 272, 521, 709, 162, 446, 714, 238, 499, 'OVERALL', 'UPS', 2014);
INSERT INTO workerpopulationratio (id, rural_male, rural_female, rural_person, urban_male, urban_female, urban_person, rural_urban_male, rural_urban_female, rural_urban_person, caste, type, year) VALUES (30, 743, 351, 571, 714, 175, 455, 735, 296, 537, 'OVERALL', 'UPSS', 2014);
INSERT INTO workerpopulationratio (id, rural_male, rural_female, rural_person, urban_male, urban_female, urban_person, rural_urban_male, rural_urban_female, rural_urban_person, caste, type, year) VALUES (5, 768, 251, 524, 731, 190, 471, 761, 238, 513, 'SC', 'UPS', 2013);
INSERT INTO workerpopulationratio (id, rural_male, rural_female, rural_person, urban_male, urban_female, urban_person, rural_urban_male, rural_urban_female, rural_urban_person, caste, type, year) VALUES (6, 767, 351, 567, 688, 181, 443, 755, 324, 547, 'ST', 'UPS', 2013);
INSERT INTO workerpopulationratio (id, rural_male, rural_female, rural_person, urban_male, urban_female, urban_person, rural_urban_male, rural_urban_female, rural_urban_person, caste, type, year) VALUES (7, 745, 227, 500, 717, 152, 446, 738, 207, 485, 'OBC', 'UPS', 2013);
INSERT INTO workerpopulationratio (id, rural_male, rural_female, rural_person, urban_male, urban_female, urban_person, rural_urban_male, rural_urban_female, rural_urban_person, caste, type, year) VALUES (8, 733, 180, 471, 675, 124, 413, 711, 159, 449, 'OTHERS', 'UPS', 2013);
INSERT INTO workerpopulationratio (id, rural_male, rural_female, rural_person, urban_male, urban_female, urban_person, rural_urban_male, rural_urban_female, rural_urban_person, caste, type, year) VALUES (27, 749, 234, 505, 699, 147, 435, 735, 209, 485, 'OVERALL', 'UPS', 2013);
