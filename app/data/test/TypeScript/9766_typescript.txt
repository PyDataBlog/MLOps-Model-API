import { MonthList } from '../enums/MonthList'

function handleMonth(month: string | number): number {
    if (typeof month === 'string') {
        return MonthList[month]
    } else {
        return month
    }
}

export default handleMonth
