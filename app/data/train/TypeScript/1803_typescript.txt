export interface IChoice<T> {
  value: T,
  label?: string
}

export class Choice<T> implements IChoice<T> {
  private _label;

  constructor(public value: T, label?: string) {
    this._label = label;
  }

  get label(): string {
    return this._label || String(this.value);
  }

  toJs(): object {
    return {
      label: this.label,
      value: this.value
    }
  }

  fromJs(obj): Choice<T> {
    return new Choice(obj.value, obj.label);
  }
}