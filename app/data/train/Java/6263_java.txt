package ru.revdaalex.lsp.chapterI.storage;

import ru.revdaalex.lsp.chapterI.food.Food;
import ru.revdaalex.lsp.chapterI.interfaces.Storage;

import java.util.ArrayList;

/**
 * Warehouse class.
 * Created by revdaalex on 04.07.2016.
 */
public class Warehouse implements Storage {
    /**
     * ArrayList Warehouse.
     */
    private final ArrayList<Food> warehouse = new ArrayList<Food>();

    /**
     * Implements interface method add in Warehouse.
     * @param food
     */
    public void add(Food food) {
        this.warehouse.add(food);
    }

    /**
     * Implements interface method sortQuality in Warehouse.
     * @param food
     * @return
     */
    public boolean sortQuality(Food food) {
        if (food.getExpiryDateInPercents() < 25){
            return true;
        }
        return false;
    }

    public ArrayList<Food> getFood() {
        return warehouse;
    }
}
