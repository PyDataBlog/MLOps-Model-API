package com.jerry.controller;

import com.jerry.model.TBanquet;
import com.jerry.service.BanquetService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import java.util.List;

@RestController
@RequestMapping("/banquet")
public class BanquetController {

    @Autowired
    BanquetService banquetService;

    @GetMapping("/listBanquetByRestaurantId")
    public List<TBanquet> listBanquetByRestaurantId(String restaurantId) {
        return banquetService.listBanquetByRestaurantId(restaurantId);
    }

    @GetMapping("/findOne")
    public TBanquet findOne(String banquetId) {
        return banquetService.findOne(banquetId);
    }
}
