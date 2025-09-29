impl get_base_price.get {
    report here.core_info.price;
}

impl calculate_discount.calculate {
    discount_amount = here.core_info.price * here.discount.discount_rate;
    report discount_amount;
}

impl calculate_tax.calculate {
    tax_rate = 0.08;
    tax_amount = here.core_info.price * tax_rate;
    report tax_amount;
}

impl calculate_final_price.calculate {
    base_price = here.core_info.price;
    discount_amount = base_price * here.discount.discount_rate;
    tax_amount = (base_price - discount_amount) * 0.08;
    final_price = base_price - discount_amount + tax_amount;
    report final_price;
}

impl calculate_shipping.calculate {
    base_shipping = 5.0;  # Base shipping cost
    weight_cost = here.shipping.weight * 0.5;  # $0.5 per unit weight
    
    if here.shipping.shipping_class == "express" {
        shipping_cost = (base_shipping + weight_cost) * 2;
    } elif here.shipping.shipping_class == "overnight" {
        shipping_cost = (base_shipping + weight_cost) * 3;
    } else {
        shipping_cost = base_shipping + weight_cost;
    }
    
    if here.shipping.is_fragile {
        shipping_cost += 10.0;  # Fragile handling fee
    }
    
    report shipping_cost;
}

impl is_eligible_for_free_shipping.is_eligible {
    # Free shipping for orders over $50 or lightweight items
    is_eligible = here.core_info.price >= 50.0 or here.shipping.weight <= 1.0;
    report is_eligible;
}

impl calculate_shipping_weight.calculate {
    total_weight = here.shipping.weight * here.core_info.quantity;
    report total_weight;
}

impl apply_coupon.apply {
    if self.coupon_code in here.discount.eligible_coupons {
        # Apply 15% additional discount for valid coupons
        coupon_discount = here.core_info.price * 0.15;
        report coupon_discount;
    } else {
        report 0.0;
    }
}

impl is_discount_applicable.is_discount {
    # Discount applicable if discount rate > 0 and item quantity > 0
    is_applicable = here.discount.discount_rate > 0 and here.core_info.quantity > 0;
    report is_applicable;
}

impl apply_bulk_discount.apply {
    if self.quantity >= here.discount.bulk_discount_threshold {
        bulk_discount = here.discount.bulk_discount_amount * self.quantity;
        report bulk_discount;
    } else {
        report 0.0;
    }
}

impl is_gift_eligible.is_gift {
    report here.gift_option.is_gift_eligible;
}

impl calculate_gift_wrap_fee.calculate {
    if here.gift_option.is_gift_eligible {
        report here.gift_option.gift_wrap_price;
    } else {
        report 0.0;
    }
}

impl get_category.get {
    report here.core_info.category;
}

impl get_tags.get {
    report here.core_info.tags;
}

impl get_inventory_status.get {
    if here.inventory.stock_level > 0 {
        report "In Stock";
    } elif here.inventory.backorder_allowed {
        report "Available for Backorder";
    } else {
        report "Out of Stock";
    }
}