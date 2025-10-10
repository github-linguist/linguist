
import datetime;

obj Metadata {
    has brand:str;
    has model_number:str;
    has release_date:str;
    has echo_friendly:bool;
    has digital_download:bool;
}

obj Gift {
    has is_gift_eligible:bool;
    has gift_wrap_price:float;
    has gift_message_allowed:bool;
}

obj Inventory {
    has stock_level:int;
    has backorder_allowed:bool;
    has estimated_restock_date:str;
}

obj Discounts {
    has discount_rate:float;
    has eligible_coupons:list[str];
    has bulk_discount_threshold:int;
    has bulk_discount_amount:float;
}

obj Shipping {
    has weight:float;
    has dimensions:tuple;
    has is_fragile:bool;
    has shipping_class:str;
    has origin_location:str;
}

obj CoreInfo {
    has id:str;
    has name:str;
    has category:str;
    has price:float;
    has quantity:int;
    has tags:list[str] = [];

}

obj Pricing {
    has tax_rate: float = 0;
    has price: float = 0;
    has discount_rate: float = 0;
    has final_price: float = 0;
}

node Item {
    has core_info:CoreInfo;
    has shipping:Shipping;
    has discount:Discounts;
    has inventory:Inventory;
    has gift_option:Gift;
    has metadata:Metadata;
    has pricing: Pricing;
}

walker create_item {
    has core_info:CoreInfo;
    has shipping:Shipping;
    has discount:Discounts;
    has inventory:Inventory;
    has gift_option:Gift;
    has metadata:Metadata;
    has tax_rate: float;

    can create with `root entry {
        final_price = self.core_info.price*(1-self.discount.discount_rate + self.tax_rate);
        pricing = Pricing(self.tax_rate,self.core_info.price, self.discount.discount_rate,final_price);
        root ++> Item(self.core_info, self.shipping, self.discount, self.inventory, self.gift_option, self.metadata, pricing);
        print([root -->]);
        report [root -->];
    }
}

walker get_base_price {
    can get with Item entry;
}

walker calculate_discount {
    can calculate with Item entry;
}

walker calculate_tax {
    can calculate with Item entry;
}

walker calculate_final_price {
    can calculate with Item entry;
}

walker calculate_shipping {
    can calculate with Item entry;
}

walker is_eligible_for_free_shipping {
    can is_eligible with Item entry;
}

walker calculate_shipping_weight {
    can calculate with Item entry;
}

walker apply_coupon {
    has coupon_code:str="";

    can apply with Item entry;
}

walker is_discount_applicable {
    can is_discount with Item entry;
}

walker apply_bulk_discount {
    has quantity:int;
    can apply with Item entry;
}

walker is_gift_eligible {
    can is_gift with Item entry;
}

walker calculate_gift_wrap_fee {
    can calculate with Item entry;
}

walker get_category {
    can get with Item entry;
}

walker get_tags {
    can get with Item entry;
}

walker get_inventory_status {
    can get with Item entry;
}