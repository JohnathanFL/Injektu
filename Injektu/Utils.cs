using System.Collections.Generic;
using System.Linq;

namespace Injektu.Utils
{
    public static class Utils
    {
        public static Dictionary<TKey, TVal> CloneDictionary<TKey, TVal>(this Dictionary<TKey, TVal> self) where TKey : notnull => self.ToDictionary(_ => _.Key, _ => _.Value);
    }
}