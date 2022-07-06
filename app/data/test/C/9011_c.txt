#include <linux/module.h>
#include <linux/init.h>

MODULE_LICENSE("Dual MIT/GPL");

static int symple_driver_init(void)
{
	printk(KERN_ALERT "driver loaded\n");
	return 0;
}

static void simple_driver_exit(void)
{
	printk(KERN_ALERT "driver unloaded\n");
}

module_init(symple_driver_init);
module_exit(simple_driver_exit); 
