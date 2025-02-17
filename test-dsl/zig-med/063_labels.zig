const print = @import("std").debug.print;

const ingredients = 4;
const foods = 4;

const Food = struct {
    name: []const u8,
    requires: [ingredients]bool,
};


const menu: [foods]Food = [_]Food{
    Food{
        .name = "Mac & Cheese",
        .requires = [ingredients]bool{ false, true, false, true },
    },
    Food{
        .name = "Chili Mac",
        .requires = [ingredients]bool{ true, true, false, false },
    },
    Food{
        .name = "Pasta",
        .requires = [ingredients]bool{ false, true, true, false },
    },
    Food{
        .name = "Cheesy Chili",
        .requires = [ingredients]bool{ true, false, false, true },
    },
};

pub fn main() void {
    const wanted_ingredients = [_]u8{ 0, 3 }; // Chili, Cheese

    const meal = food_loop: for (menu) |food| {

        for (food.requires, 0..) |required, required_ingredient| {

            if (!required) continue;

            const found = for (wanted_ingredients) |want_it| {
                if (required_ingredient == want_it) break true;
            } else false;

            if (!found) continue :food_loop;
        }

        break food;
    } else menu[0];

    print("Enjoy your {s}!\n", .{meal.name});
}

