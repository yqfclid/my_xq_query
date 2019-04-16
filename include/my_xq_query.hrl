-define(COOKIE_TAB, cookie_tab).
-define(SYMBOLS_TAB, symbols_tab).

-define(XQ_URL, <<"https://xueqiu.com">>).
-define(MARKET_URL, <<"https://xueqiu.com/v4/stock/quote.json?code=">>).

-define(TRADING_DURATION, [{{9, 29, 50}, {11, 30, 10}},
                           {{12, 59, 50}, {15, 00, 10}}]).

-record(market, {symbol,
                 date,
                 time,
                 detail}).