'use strict';

import express = require('express');
const router: express.Router = express.Router();

const service = require('../../services').GalleryService;

router.get('/:limit', service.getAll);
router.get('/main/:limit', service.getAll);
router.get('/main/random/:limit', service.getRandomGallery);
router.get('/main/popular/:limit', service.getPopularGallery);
router.get('/category/:id/:itemid', service.getGalleries);
router.get('/single/random', service.getRandomCollection);
router.get('/single/:id/:catid/:itemid', service.getCollection);

module.exports = router;