## Navbar 导航栏

### 使用指南
``` javascript
import { NavBar } from 'icui';

Vue.component(NavBar.name, NavBar);
```

### 代码演示

#### 

:::demo
```html
<ic-navbar
    title="NavBar"
    leftContent="返回"
    leftIcon
    rightIcon
></ic-navbar>
```
:::

### Options

| 参数       | 说明      | 类型       | 默认值       |
|-----------|-----------|-----------|-------------|
| title | 标题 | `String` | false|
| leftContent | 返回 | `String` | false|
| leftIcon | 返回按钮 | `Bolearn` | true|

