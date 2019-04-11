-define(COOKIE_TAB, cookie_tab).
-define(SYMBOLS_TAB, symbols_tab).

-define(XQ_URL, <<"https://xueqiu.com">>).
-define(MARKET_URL, <<"https://xueqiu.com/v4/stock/quote.json?code=">>).


-record(market, {symbol,
                 date,
                 time,
                 detail}).