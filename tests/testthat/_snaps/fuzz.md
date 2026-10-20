# check object returned

    Code
      fuzz()
    Condition
      Error in `fuzz()`:
      ! `funs` should be of class <character>

---

    Code
      fuzz("mean", what = NULL, args = NULL)
    Condition
      Error in `fuzz()`:
      ! `what` and `args` cannot be both NULL

---

    Code
      fuzz("mean", timeout = 0)
    Condition
      Error in `fuzz()`:
      ! `timeout` should be at least 0.5

# worker dying during a fuzz run

    Code
      fuzz("mean", list(NA))
    Message
      i Fuzzing 1 function with 1 input (using 0 daemons)
      i Functions will be searched in the global namespace as `package` was not specified
    Condition
      Error in `fuzz()`:
      ! No live daemons available to run the tests, restart them with `mirai::daemons()`

# self fuzz

    Code
      res$runs
    Output
          idx res                                                  msg  fun
      1     1  OK                `funs` should be of class <character> fuzz
      2     2  OK                `funs` should be of class <character> fuzz
      3     3  OK                `funs` should be of class <character> fuzz
      4     4  OK                `funs` should be of class <character> fuzz
      5     5  OK                `funs` should be of class <character> fuzz
      6     6  OK                `funs` should be of class <character> fuzz
      7     7  OK                `funs` should be of class <character> fuzz
      8     8  OK                `funs` should be of class <character> fuzz
      9     9  OK                `funs` should be of class <character> fuzz
      10   10  OK                `funs` should be of class <character> fuzz
      11   11  OK                `funs` should be of class <character> fuzz
      12   12  OK                `funs` should be of class <character> fuzz
      13   13  OK                `funs` should be of class <character> fuzz
      14   14  OK                `funs` should be of class <character> fuzz
      15   15  OK                `funs` should be of class <character> fuzz
      16   16  OK                `funs` should be of class <character> fuzz
      17   17  OK                `funs` should be of class <character> fuzz
      18   18  OK                `funs` should be of class <character> fuzz
      19   19  OK                `funs` should be of class <character> fuzz
      20   20  OK                `funs` should be of class <character> fuzz
      21   21  OK                `funs` should be of class <character> fuzz
      22   22  OK                `funs` should be of class <character> fuzz
      23   23  OK                `funs` should be of class <character> fuzz
      24   24  OK                `funs` should be of class <character> fuzz
      25   25  OK                `funs` should be of class <character> fuzz
      26   26  OK                `funs` should be of class <character> fuzz
      27   27  OK                `funs` should be of class <character> fuzz
      28   28  OK                `funs` should be of class <character> fuzz
      29   29  OK                                                      fuzz
      30   30  OK                       `funs` is an empty <character> fuzz
      31   31  OK                                                      fuzz
      32   32  OK                                                      fuzz
      33   33  OK                                                      fuzz
      34   34  OK                       `funs` is an empty <character> fuzz
      35   35  OK                `funs` should be of class <character> fuzz
      36   36  OK                `funs` should be of class <character> fuzz
      37   37  OK                `funs` should be of class <character> fuzz
      38   38  OK                `funs` should be of class <character> fuzz
      39   39  OK                `funs` should be of class <character> fuzz
      40   40  OK                `funs` should be of class <character> fuzz
      41   41  OK                `funs` should be of class <character> fuzz
      42   42  OK                `funs` should be of class <character> fuzz
      43   43  OK                `funs` should be of class <character> fuzz
      44   44  OK                `funs` should be of class <character> fuzz
      45   45  OK                `funs` should be of class <character> fuzz
      46   46  OK                `funs` should be of class <character> fuzz
      47   47  OK                `funs` should be of class <character> fuzz
      48   48  OK                `funs` should be of class <character> fuzz
      49   49  OK                `funs` should be of class <character> fuzz
      50   50  OK                `funs` should be of class <character> fuzz
      51   51  OK                `funs` should be of class <character> fuzz
      52   52  OK                `funs` should be of class <character> fuzz
      53   53  OK                `funs` should be of class <character> fuzz
      54   54  OK                `funs` should be of class <character> fuzz
      55   55  OK                `funs` should be of class <character> fuzz
      56   56  OK                `funs` should be of class <character> fuzz
      57   57  OK                `funs` should be of class <character> fuzz
      58   58  OK                `funs` should be of class <character> fuzz
      59   59  OK                `funs` should be of class <character> fuzz
      60   60  OK                `funs` should be of class <character> fuzz
      61   61  OK                `funs` should be of class <character> fuzz
      62   62  OK                `funs` should be of class <character> fuzz
      63   63  OK                `funs` should be of class <character> fuzz
      64   64  OK                `funs` should be of class <character> fuzz
      65   65  OK                `funs` should be of class <character> fuzz
      66   66  OK                `funs` should be of class <character> fuzz
      67   67  OK                `funs` should be of class <character> fuzz
      68   68  OK                `funs` should be of class <character> fuzz
      69   69  OK                `funs` should be of class <character> fuzz
      70   70  OK                `funs` should be of class <character> fuzz
      71   71  OK                `funs` should be of class <character> fuzz
      72   72  OK                `funs` should be of class <character> fuzz
      73   73  OK                `funs` should be of class <character> fuzz
      74   74  OK                `funs` should be of class <character> fuzz
      75   75  OK                `funs` should be of class <character> fuzz
      76   76  OK                `funs` should be of class <character> fuzz
      77   77  OK                `funs` should be of class <character> fuzz
      78   78  OK                `funs` should be of class <character> fuzz
      79   79  OK                `funs` should be of class <character> fuzz
      80   80  OK                `funs` should be of class <character> fuzz
      81   81  OK                `funs` should be of class <character> fuzz
      82   82  OK                `funs` should be of class <character> fuzz
      83   83  OK                `funs` should be of class <character> fuzz
      84   84  OK                `funs` should be of class <character> fuzz
      85   85  OK                `funs` should be of class <character> fuzz
      86   86  OK                `funs` should be of class <character> fuzz
      87   87  OK                `funs` should be of class <character> fuzz
      88   88  OK                `funs` should be of class <character> fuzz
      89   89  OK                `funs` should be of class <character> fuzz
      90   90  OK                `funs` should be of class <character> fuzz
      91   91  OK                `funs` should be of class <character> fuzz
      92   92  OK                `funs` should be of class <character> fuzz
      93   93  OK                `funs` should be of class <character> fuzz
      94   94  OK                `funs` should be of class <character> fuzz
      95   95  OK                `funs` should be of class <character> fuzz
      96   96  OK             `what` should be of class <list> or NULL fuzz
      97   97  OK             `what` should be of class <list> or NULL fuzz
      98   98  OK             `what` should be of class <list> or NULL fuzz
      99   99  OK             `what` should be of class <list> or NULL fuzz
      100 100  OK             `what` should be of class <list> or NULL fuzz
      101 101  OK             `what` should be of class <list> or NULL fuzz
      102 102  OK             `what` should be of class <list> or NULL fuzz
      103 103  OK             `what` should be of class <list> or NULL fuzz
      104 104  OK             `what` should be of class <list> or NULL fuzz
      105 105  OK             `what` should be of class <list> or NULL fuzz
      106 106  OK             `what` should be of class <list> or NULL fuzz
      107 107  OK             `what` should be of class <list> or NULL fuzz
      108 108  OK                `what` and `args` cannot be both NULL fuzz
      109 109  OK             `what` should be of class <list> or NULL fuzz
      110 110  OK             `what` should be of class <list> or NULL fuzz
      111 111  OK             `what` should be of class <list> or NULL fuzz
      112 112  OK             `what` should be of class <list> or NULL fuzz
      113 113  OK             `what` should be of class <list> or NULL fuzz
      114 114  OK             `what` should be of class <list> or NULL fuzz
      115 115  OK             `what` should be of class <list> or NULL fuzz
      116 116  OK             `what` should be of class <list> or NULL fuzz
      117 117  OK             `what` should be of class <list> or NULL fuzz
      118 118  OK             `what` should be of class <list> or NULL fuzz
      119 119  OK             `what` should be of class <list> or NULL fuzz
      120 120  OK             `what` should be of class <list> or NULL fuzz
      121 121  OK             `what` should be of class <list> or NULL fuzz
      122 122  OK             `what` should be of class <list> or NULL fuzz
      123 123  OK             `what` should be of class <list> or NULL fuzz
      124 124  OK             `what` should be of class <list> or NULL fuzz
      125 125  OK             `what` should be of class <list> or NULL fuzz
      126 126  OK             `what` should be of class <list> or NULL fuzz
      127 127  OK             `what` should be of class <list> or NULL fuzz
      128 128  OK             `what` should be of class <list> or NULL fuzz
      129 129  OK             `what` should be of class <list> or NULL fuzz
      130 130  OK             `what` should be of class <list> or NULL fuzz
      131 131  OK             `what` should be of class <list> or NULL fuzz
      132 132  OK             `what` should be of class <list> or NULL fuzz
      133 133  OK             `what` should be of class <list> or NULL fuzz
      134 134  OK             `what` should be of class <list> or NULL fuzz
      135 135  OK             `what` should be of class <list> or NULL fuzz
      136 136  OK             `what` should be of class <list> or NULL fuzz
      137 137  OK             `what` should be of class <list> or NULL fuzz
      138 138  OK             `what` should be of class <list> or NULL fuzz
      139 139  OK             `what` should be of class <list> or NULL fuzz
      140 140  OK             `what` should be of class <list> or NULL fuzz
      141 141  OK             `what` should be of class <list> or NULL fuzz
      142 142  OK             `what` should be of class <list> or NULL fuzz
      143 143  OK             `what` should be of class <list> or NULL fuzz
      144 144  OK             `what` should be of class <list> or NULL fuzz
      145 145  OK             `what` should be of class <list> or NULL fuzz
      146 146  OK             `what` should be of class <list> or NULL fuzz
      147 147  OK             `what` should be of class <list> or NULL fuzz
      148 148  OK             `what` should be of class <list> or NULL fuzz
      149 149  OK             `what` should be of class <list> or NULL fuzz
      150 150  OK             `what` should be of class <list> or NULL fuzz
      151 151  OK             `what` should be of class <list> or NULL fuzz
      152 152  OK             `what` should be of class <list> or NULL fuzz
      153 153  OK             `what` should be of class <list> or NULL fuzz
      154 154  OK             `what` should be of class <list> or NULL fuzz
      155 155  OK             `what` should be of class <list> or NULL fuzz
      156 156  OK             `what` should be of class <list> or NULL fuzz
      157 157  OK             `what` should be of class <list> or NULL fuzz
      158 158  OK             `what` should be of class <list> or NULL fuzz
      159 159  OK             `what` should be of class <list> or NULL fuzz
      160 160  OK             `what` should be of class <list> or NULL fuzz
      161 161  OK             `what` should be of class <list> or NULL fuzz
      162 162  OK             `what` should be of class <list> or NULL fuzz
      163 163  OK             `what` should be of class <list> or NULL fuzz
      164 164  OK             `what` should be of class <list> or NULL fuzz
      165 165  OK             `what` should be of class <list> or NULL fuzz
      166 166  OK             `what` should be of class <list> or NULL fuzz
      167 167  OK             `what` should be of class <list> or NULL fuzz
      168 168  OK             `what` should be of class <list> or NULL fuzz
      169 169  OK             `what` should be of class <list> or NULL fuzz
      170 170  OK             `what` should be of class <list> or NULL fuzz
      171 171  OK             `what` should be of class <list> or NULL fuzz
      172 172  OK             `what` should be of class <list> or NULL fuzz
      173 173  OK             `what` should be of class <list> or NULL fuzz
      174 174  OK             `what` should be of class <list> or NULL fuzz
      175 175  OK             `what` should be of class <list> or NULL fuzz
      176 176  OK             `what` should be of class <list> or NULL fuzz
      177 177  OK             `what` should be of class <list> or NULL fuzz
      178 178  OK             `what` should be of class <list> or NULL fuzz
      179 179  OK             `what` should be of class <list> or NULL fuzz
      180 180  OK             `what` should be of class <list> or NULL fuzz
      181 181  OK             `what` should be of class <list> or NULL fuzz
      182 182  OK             `what` should be of class <list> or NULL fuzz
      183 183  OK             `what` should be of class <list> or NULL fuzz
      184 184  OK             `what` should be of class <list> or NULL fuzz
      185 185  OK             `what` should be of class <list> or NULL fuzz
      186 186  OK             `what` should be of class <list> or NULL fuzz
      187 187  OK             `what` should be of class <list> or NULL fuzz
      188 188  OK                                                      fuzz
      189 189  OK                                                      fuzz
      190 190  OK                            `what` is an empty <list> fuzz
      191 191  OK             `args` should be of class <list> or NULL fuzz
      192 192  OK             `args` should be of class <list> or NULL fuzz
      193 193  OK             `args` should be of class <list> or NULL fuzz
      194 194  OK             `args` should be of class <list> or NULL fuzz
      195 195  OK             `args` should be of class <list> or NULL fuzz
      196 196  OK             `args` should be of class <list> or NULL fuzz
      197 197  OK             `args` should be of class <list> or NULL fuzz
      198 198  OK             `args` should be of class <list> or NULL fuzz
      199 199  OK             `args` should be of class <list> or NULL fuzz
      200 200  OK             `args` should be of class <list> or NULL fuzz
      201 201  OK             `args` should be of class <list> or NULL fuzz
      202 202  OK             `args` should be of class <list> or NULL fuzz
      203 203  OK                                                      fuzz
      204 204  OK             `args` should be of class <list> or NULL fuzz
      205 205  OK             `args` should be of class <list> or NULL fuzz
      206 206  OK             `args` should be of class <list> or NULL fuzz
      207 207  OK             `args` should be of class <list> or NULL fuzz
      208 208  OK             `args` should be of class <list> or NULL fuzz
      209 209  OK             `args` should be of class <list> or NULL fuzz
      210 210  OK             `args` should be of class <list> or NULL fuzz
      211 211  OK             `args` should be of class <list> or NULL fuzz
      212 212  OK             `args` should be of class <list> or NULL fuzz
      213 213  OK             `args` should be of class <list> or NULL fuzz
      214 214  OK             `args` should be of class <list> or NULL fuzz
      215 215  OK             `args` should be of class <list> or NULL fuzz
      216 216  OK             `args` should be of class <list> or NULL fuzz
      217 217  OK             `args` should be of class <list> or NULL fuzz
      218 218  OK             `args` should be of class <list> or NULL fuzz
      219 219  OK             `args` should be of class <list> or NULL fuzz
      220 220  OK             `args` should be of class <list> or NULL fuzz
      221 221  OK             `args` should be of class <list> or NULL fuzz
      222 222  OK             `args` should be of class <list> or NULL fuzz
      223 223  OK             `args` should be of class <list> or NULL fuzz
      224 224  OK             `args` should be of class <list> or NULL fuzz
      225 225  OK             `args` should be of class <list> or NULL fuzz
      226 226  OK             `args` should be of class <list> or NULL fuzz
      227 227  OK             `args` should be of class <list> or NULL fuzz
      228 228  OK             `args` should be of class <list> or NULL fuzz
      229 229  OK             `args` should be of class <list> or NULL fuzz
      230 230  OK             `args` should be of class <list> or NULL fuzz
      231 231  OK             `args` should be of class <list> or NULL fuzz
      232 232  OK             `args` should be of class <list> or NULL fuzz
      233 233  OK             `args` should be of class <list> or NULL fuzz
      234 234  OK             `args` should be of class <list> or NULL fuzz
      235 235  OK             `args` should be of class <list> or NULL fuzz
      236 236  OK             `args` should be of class <list> or NULL fuzz
      237 237  OK             `args` should be of class <list> or NULL fuzz
      238 238  OK             `args` should be of class <list> or NULL fuzz
      239 239  OK             `args` should be of class <list> or NULL fuzz
      240 240  OK             `args` should be of class <list> or NULL fuzz
      241 241  OK             `args` should be of class <list> or NULL fuzz
      242 242  OK             `args` should be of class <list> or NULL fuzz
      243 243  OK             `args` should be of class <list> or NULL fuzz
      244 244  OK             `args` should be of class <list> or NULL fuzz
      245 245  OK             `args` should be of class <list> or NULL fuzz
      246 246  OK             `args` should be of class <list> or NULL fuzz
      247 247  OK             `args` should be of class <list> or NULL fuzz
      248 248  OK             `args` should be of class <list> or NULL fuzz
      249 249  OK             `args` should be of class <list> or NULL fuzz
      250 250  OK             `args` should be of class <list> or NULL fuzz
      251 251  OK             `args` should be of class <list> or NULL fuzz
      252 252  OK             `args` should be of class <list> or NULL fuzz
      253 253  OK             `args` should be of class <list> or NULL fuzz
      254 254  OK             `args` should be of class <list> or NULL fuzz
      255 255  OK             `args` should be of class <list> or NULL fuzz
      256 256  OK             `args` should be of class <list> or NULL fuzz
      257 257  OK             `args` should be of class <list> or NULL fuzz
      258 258  OK             `args` should be of class <list> or NULL fuzz
      259 259  OK             `args` should be of class <list> or NULL fuzz
      260 260  OK             `args` should be of class <list> or NULL fuzz
      261 261  OK             `args` should be of class <list> or NULL fuzz
      262 262  OK             `args` should be of class <list> or NULL fuzz
      263 263  OK             `args` should be of class <list> or NULL fuzz
      264 264  OK             `args` should be of class <list> or NULL fuzz
      265 265  OK             `args` should be of class <list> or NULL fuzz
      266 266  OK             `args` should be of class <list> or NULL fuzz
      267 267  OK             `args` should be of class <list> or NULL fuzz
      268 268  OK             `args` should be of class <list> or NULL fuzz
      269 269  OK             `args` should be of class <list> or NULL fuzz
      270 270  OK             `args` should be of class <list> or NULL fuzz
      271 271  OK             `args` should be of class <list> or NULL fuzz
      272 272  OK             `args` should be of class <list> or NULL fuzz
      273 273  OK             `args` should be of class <list> or NULL fuzz
      274 274  OK             `args` should be of class <list> or NULL fuzz
      275 275  OK             `args` should be of class <list> or NULL fuzz
      276 276  OK             `args` should be of class <list> or NULL fuzz
      277 277  OK             `args` should be of class <list> or NULL fuzz
      278 278  OK             `args` should be of class <list> or NULL fuzz
      279 279  OK             `args` should be of class <list> or NULL fuzz
      280 280  OK             `args` should be of class <list> or NULL fuzz
      281 281  OK             `args` should be of class <list> or NULL fuzz
      282 282  OK             `args` should be of class <list> or NULL fuzz
      283 283  OK                                                      fuzz
      284 284  OK                                                      fuzz
      285 285  OK                            `args` is an empty <list> fuzz
      286 286  OK             `package` should be of class <character> fuzz
      287 287  OK             `package` should be of class <character> fuzz
      288 288  OK             `package` should be of class <character> fuzz
      289 289  OK             `package` should be of class <character> fuzz
      290 290  OK             `package` should be of class <character> fuzz
      291 291  OK             `package` should be of class <character> fuzz
      292 292  OK             `package` should be of class <character> fuzz
      293 293  OK             `package` should be of class <character> fuzz
      294 294  OK             `package` should be of class <character> fuzz
      295 295  OK             `package` should be of class <character> fuzz
      296 296  OK             `package` should be of class <character> fuzz
      297 297  OK             `package` should be of class <character> fuzz
      298 298  OK             `package` should be of class <character> fuzz
      299 299  OK             `package` should be of class <character> fuzz
      300 300  OK             `package` should be of class <character> fuzz
      301 301  OK             `package` should be of class <character> fuzz
      302 302  OK             `package` should be of class <character> fuzz
      303 303  OK             `package` should be of class <character> fuzz
      304 304  OK             `package` should be of class <character> fuzz
      305 305  OK             `package` should be of class <character> fuzz
      306 306  OK             `package` should be of class <character> fuzz
      307 307  OK             `package` should be of class <character> fuzz
      308 308  OK             `package` should be of class <character> fuzz
      309 309  OK             `package` should be of class <character> fuzz
      310 310  OK             `package` should be of class <character> fuzz
      311 311  OK             `package` should be of class <character> fuzz
      312 312  OK             `package` should be of class <character> fuzz
      313 313  OK       `package` should be a single <character> value fuzz
      314 314  OK                    `package` is an empty <character> fuzz
      315 315  OK                                                      fuzz
      316 316  OK       `package` should be a single <character> value fuzz
      317 317  OK       `package` should be a single <character> value fuzz
      318 318  OK                    `package` is an empty <character> fuzz
      319 319  OK             `package` should be of class <character> fuzz
      320 320  OK             `package` should be of class <character> fuzz
      321 321  OK             `package` should be of class <character> fuzz
      322 322  OK             `package` should be of class <character> fuzz
      323 323  OK             `package` should be of class <character> fuzz
      324 324  OK             `package` should be of class <character> fuzz
      325 325  OK             `package` should be of class <character> fuzz
      326 326  OK             `package` should be of class <character> fuzz
      327 327  OK             `package` should be of class <character> fuzz
      328 328  OK             `package` should be of class <character> fuzz
      329 329  OK             `package` should be of class <character> fuzz
      330 330  OK             `package` should be of class <character> fuzz
      331 331  OK             `package` should be of class <character> fuzz
      332 332  OK             `package` should be of class <character> fuzz
      333 333  OK             `package` should be of class <character> fuzz
      334 334  OK             `package` should be of class <character> fuzz
      335 335  OK             `package` should be of class <character> fuzz
      336 336  OK             `package` should be of class <character> fuzz
      337 337  OK             `package` should be of class <character> fuzz
      338 338  OK             `package` should be of class <character> fuzz
      339 339  OK             `package` should be of class <character> fuzz
      340 340  OK             `package` should be of class <character> fuzz
      341 341  OK             `package` should be of class <character> fuzz
      342 342  OK             `package` should be of class <character> fuzz
      343 343  OK             `package` should be of class <character> fuzz
      344 344  OK             `package` should be of class <character> fuzz
      345 345  OK             `package` should be of class <character> fuzz
      346 346  OK             `package` should be of class <character> fuzz
      347 347  OK             `package` should be of class <character> fuzz
      348 348  OK             `package` should be of class <character> fuzz
      349 349  OK             `package` should be of class <character> fuzz
      350 350  OK             `package` should be of class <character> fuzz
      351 351  OK             `package` should be of class <character> fuzz
      352 352  OK             `package` should be of class <character> fuzz
      353 353  OK             `package` should be of class <character> fuzz
      354 354  OK             `package` should be of class <character> fuzz
      355 355  OK             `package` should be of class <character> fuzz
      356 356  OK             `package` should be of class <character> fuzz
      357 357  OK             `package` should be of class <character> fuzz
      358 358  OK             `package` should be of class <character> fuzz
      359 359  OK             `package` should be of class <character> fuzz
      360 360  OK             `package` should be of class <character> fuzz
      361 361  OK             `package` should be of class <character> fuzz
      362 362  OK             `package` should be of class <character> fuzz
      363 363  OK             `package` should be of class <character> fuzz
      364 364  OK             `package` should be of class <character> fuzz
      365 365  OK             `package` should be of class <character> fuzz
      366 366  OK             `package` should be of class <character> fuzz
      367 367  OK             `package` should be of class <character> fuzz
      368 368  OK             `package` should be of class <character> fuzz
      369 369  OK             `package` should be of class <character> fuzz
      370 370  OK             `package` should be of class <character> fuzz
      371 371  OK             `package` should be of class <character> fuzz
      372 372  OK             `package` should be of class <character> fuzz
      373 373  OK             `package` should be of class <character> fuzz
      374 374  OK             `package` should be of class <character> fuzz
      375 375  OK             `package` should be of class <character> fuzz
      376 376  OK             `package` should be of class <character> fuzz
      377 377  OK             `package` should be of class <character> fuzz
      378 378  OK             `package` should be of class <character> fuzz
      379 379  OK             `package` should be of class <character> fuzz
      380 380  OK          `listify_what` should be of class <logical> fuzz
      381 381  OK          `listify_what` should be of class <logical> fuzz
      382 382  OK          `listify_what` should be of class <logical> fuzz
      383 383  OK          `listify_what` should be of class <logical> fuzz
      384 384  OK          `listify_what` should be of class <logical> fuzz
      385 385  OK          `listify_what` should be of class <logical> fuzz
      386 386  OK          `listify_what` should be of class <logical> fuzz
      387 387  OK          `listify_what` should be of class <logical> fuzz
      388 388  OK          `listify_what` should be of class <logical> fuzz
      389 389  OK          `listify_what` should be of class <logical> fuzz
      390 390  OK          `listify_what` should be of class <logical> fuzz
      391 391  OK          `listify_what` should be of class <logical> fuzz
      392 392  OK          `listify_what` should be of class <logical> fuzz
      393 393  OK          `listify_what` should be of class <logical> fuzz
      394 394  OK          `listify_what` should be of class <logical> fuzz
      395 395  OK          `listify_what` should be of class <logical> fuzz
      396 396  OK          `listify_what` should be of class <logical> fuzz
      397 397  OK          `listify_what` should be of class <logical> fuzz
      398 398  OK          `listify_what` should be of class <logical> fuzz
      399 399  OK          `listify_what` should be of class <logical> fuzz
      400 400  OK          `listify_what` should be of class <logical> fuzz
      401 401  OK          `listify_what` should be of class <logical> fuzz
      402 402  OK          `listify_what` should be of class <logical> fuzz
      403 403  OK          `listify_what` should be of class <logical> fuzz
      404 404  OK          `listify_what` should be of class <logical> fuzz
      405 405  OK                                                      fuzz
      406 406  OK    `listify_what` should be a single <logical> value fuzz
      407 407  OK                 `listify_what` is an empty <logical> fuzz
      408 408  OK          `listify_what` should be of class <logical> fuzz
      409 409  OK          `listify_what` should be of class <logical> fuzz
      410 410  OK          `listify_what` should be of class <logical> fuzz
      411 411  OK          `listify_what` should be of class <logical> fuzz
      412 412  OK          `listify_what` should be of class <logical> fuzz
      413 413  OK          `listify_what` should be of class <logical> fuzz
      414 414  OK          `listify_what` should be of class <logical> fuzz
      415 415  OK          `listify_what` should be of class <logical> fuzz
      416 416  OK          `listify_what` should be of class <logical> fuzz
      417 417  OK          `listify_what` should be of class <logical> fuzz
      418 418  OK          `listify_what` should be of class <logical> fuzz
      419 419  OK          `listify_what` should be of class <logical> fuzz
      420 420  OK          `listify_what` should be of class <logical> fuzz
      421 421  OK          `listify_what` should be of class <logical> fuzz
      422 422  OK          `listify_what` should be of class <logical> fuzz
      423 423  OK          `listify_what` should be of class <logical> fuzz
      424 424  OK          `listify_what` should be of class <logical> fuzz
      425 425  OK          `listify_what` should be of class <logical> fuzz
      426 426  OK          `listify_what` should be of class <logical> fuzz
      427 427  OK          `listify_what` should be of class <logical> fuzz
      428 428  OK          `listify_what` should be of class <logical> fuzz
      429 429  OK          `listify_what` should be of class <logical> fuzz
      430 430  OK          `listify_what` should be of class <logical> fuzz
      431 431  OK          `listify_what` should be of class <logical> fuzz
      432 432  OK          `listify_what` should be of class <logical> fuzz
      433 433  OK          `listify_what` should be of class <logical> fuzz
      434 434  OK          `listify_what` should be of class <logical> fuzz
      435 435  OK          `listify_what` should be of class <logical> fuzz
      436 436  OK          `listify_what` should be of class <logical> fuzz
      437 437  OK          `listify_what` should be of class <logical> fuzz
      438 438  OK          `listify_what` should be of class <logical> fuzz
      439 439  OK          `listify_what` should be of class <logical> fuzz
      440 440  OK          `listify_what` should be of class <logical> fuzz
      441 441  OK          `listify_what` should be of class <logical> fuzz
      442 442  OK          `listify_what` should be of class <logical> fuzz
      443 443  OK          `listify_what` should be of class <logical> fuzz
      444 444  OK          `listify_what` should be of class <logical> fuzz
      445 445  OK          `listify_what` should be of class <logical> fuzz
      446 446  OK          `listify_what` should be of class <logical> fuzz
      447 447  OK          `listify_what` should be of class <logical> fuzz
      448 448  OK          `listify_what` should be of class <logical> fuzz
      449 449  OK          `listify_what` should be of class <logical> fuzz
      450 450  OK          `listify_what` should be of class <logical> fuzz
      451 451  OK          `listify_what` should be of class <logical> fuzz
      452 452  OK          `listify_what` should be of class <logical> fuzz
      453 453  OK          `listify_what` should be of class <logical> fuzz
      454 454  OK          `listify_what` should be of class <logical> fuzz
      455 455  OK          `listify_what` should be of class <logical> fuzz
      456 456  OK          `listify_what` should be of class <logical> fuzz
      457 457  OK          `listify_what` should be of class <logical> fuzz
      458 458  OK          `listify_what` should be of class <logical> fuzz
      459 459  OK          `listify_what` should be of class <logical> fuzz
      460 460  OK          `listify_what` should be of class <logical> fuzz
      461 461  OK          `listify_what` should be of class <logical> fuzz
      462 462  OK          `listify_what` should be of class <logical> fuzz
      463 463  OK          `listify_what` should be of class <logical> fuzz
      464 464  OK          `listify_what` should be of class <logical> fuzz
      465 465  OK          `listify_what` should be of class <logical> fuzz
      466 466  OK          `listify_what` should be of class <logical> fuzz
      467 467  OK          `listify_what` should be of class <logical> fuzz
      468 468  OK          `listify_what` should be of class <logical> fuzz
      469 469  OK          `listify_what` should be of class <logical> fuzz
      470 470  OK          `listify_what` should be of class <logical> fuzz
      471 471  OK          `listify_what` should be of class <logical> fuzz
      472 472  OK          `listify_what` should be of class <logical> fuzz
      473 473  OK          `listify_what` should be of class <logical> fuzz
      474 474  OK          `listify_what` should be of class <logical> fuzz
      475 475  OK     `ignore_patterns` should be of class <character> fuzz
      476 476  OK     `ignore_patterns` should be of class <character> fuzz
      477 477  OK     `ignore_patterns` should be of class <character> fuzz
      478 478  OK     `ignore_patterns` should be of class <character> fuzz
      479 479  OK     `ignore_patterns` should be of class <character> fuzz
      480 480  OK     `ignore_patterns` should be of class <character> fuzz
      481 481  OK     `ignore_patterns` should be of class <character> fuzz
      482 482  OK     `ignore_patterns` should be of class <character> fuzz
      483 483  OK     `ignore_patterns` should be of class <character> fuzz
      484 484  OK     `ignore_patterns` should be of class <character> fuzz
      485 485  OK     `ignore_patterns` should be of class <character> fuzz
      486 486  OK     `ignore_patterns` should be of class <character> fuzz
      487 487  OK     `ignore_patterns` should be of class <character> fuzz
      488 488  OK     `ignore_patterns` should be of class <character> fuzz
      489 489  OK     `ignore_patterns` should be of class <character> fuzz
      490 490  OK     `ignore_patterns` should be of class <character> fuzz
      491 491  OK     `ignore_patterns` should be of class <character> fuzz
      492 492  OK     `ignore_patterns` should be of class <character> fuzz
      493 493  OK     `ignore_patterns` should be of class <character> fuzz
      494 494  OK     `ignore_patterns` should be of class <character> fuzz
      495 495  OK     `ignore_patterns` should be of class <character> fuzz
      496 496  OK     `ignore_patterns` should be of class <character> fuzz
      497 497  OK     `ignore_patterns` should be of class <character> fuzz
      498 498  OK     `ignore_patterns` should be of class <character> fuzz
      499 499  OK     `ignore_patterns` should be of class <character> fuzz
      500 500  OK     `ignore_patterns` should be of class <character> fuzz
      501 501  OK     `ignore_patterns` should be of class <character> fuzz
      502 502  OK     `ignore_patterns` should be of class <character> fuzz
      503 503  OK                                                      fuzz
      504 504  OK                                                      fuzz
      505 505  OK                                                      fuzz
      506 506  OK                                                      fuzz
      507 507  OK            `ignore_patterns` is an empty <character> fuzz
      508 508  OK     `ignore_patterns` should be of class <character> fuzz
      509 509  OK     `ignore_patterns` should be of class <character> fuzz
      510 510  OK     `ignore_patterns` should be of class <character> fuzz
      511 511  OK     `ignore_patterns` should be of class <character> fuzz
      512 512  OK     `ignore_patterns` should be of class <character> fuzz
      513 513  OK     `ignore_patterns` should be of class <character> fuzz
      514 514  OK     `ignore_patterns` should be of class <character> fuzz
      515 515  OK     `ignore_patterns` should be of class <character> fuzz
      516 516  OK     `ignore_patterns` should be of class <character> fuzz
      517 517  OK     `ignore_patterns` should be of class <character> fuzz
      518 518  OK     `ignore_patterns` should be of class <character> fuzz
      519 519  OK     `ignore_patterns` should be of class <character> fuzz
      520 520  OK     `ignore_patterns` should be of class <character> fuzz
      521 521  OK     `ignore_patterns` should be of class <character> fuzz
      522 522  OK     `ignore_patterns` should be of class <character> fuzz
      523 523  OK     `ignore_patterns` should be of class <character> fuzz
      524 524  OK     `ignore_patterns` should be of class <character> fuzz
      525 525  OK     `ignore_patterns` should be of class <character> fuzz
      526 526  OK     `ignore_patterns` should be of class <character> fuzz
      527 527  OK     `ignore_patterns` should be of class <character> fuzz
      528 528  OK     `ignore_patterns` should be of class <character> fuzz
      529 529  OK     `ignore_patterns` should be of class <character> fuzz
      530 530  OK     `ignore_patterns` should be of class <character> fuzz
      531 531  OK     `ignore_patterns` should be of class <character> fuzz
      532 532  OK     `ignore_patterns` should be of class <character> fuzz
      533 533  OK     `ignore_patterns` should be of class <character> fuzz
      534 534  OK     `ignore_patterns` should be of class <character> fuzz
      535 535  OK     `ignore_patterns` should be of class <character> fuzz
      536 536  OK     `ignore_patterns` should be of class <character> fuzz
      537 537  OK     `ignore_patterns` should be of class <character> fuzz
      538 538  OK     `ignore_patterns` should be of class <character> fuzz
      539 539  OK     `ignore_patterns` should be of class <character> fuzz
      540 540  OK     `ignore_patterns` should be of class <character> fuzz
      541 541  OK     `ignore_patterns` should be of class <character> fuzz
      542 542  OK     `ignore_patterns` should be of class <character> fuzz
      543 543  OK     `ignore_patterns` should be of class <character> fuzz
      544 544  OK     `ignore_patterns` should be of class <character> fuzz
      545 545  OK     `ignore_patterns` should be of class <character> fuzz
      546 546  OK     `ignore_patterns` should be of class <character> fuzz
      547 547  OK     `ignore_patterns` should be of class <character> fuzz
      548 548  OK     `ignore_patterns` should be of class <character> fuzz
      549 549  OK     `ignore_patterns` should be of class <character> fuzz
      550 550  OK     `ignore_patterns` should be of class <character> fuzz
      551 551  OK     `ignore_patterns` should be of class <character> fuzz
      552 552  OK     `ignore_patterns` should be of class <character> fuzz
      553 553  OK     `ignore_patterns` should be of class <character> fuzz
      554 554  OK     `ignore_patterns` should be of class <character> fuzz
      555 555  OK     `ignore_patterns` should be of class <character> fuzz
      556 556  OK     `ignore_patterns` should be of class <character> fuzz
      557 557  OK     `ignore_patterns` should be of class <character> fuzz
      558 558  OK     `ignore_patterns` should be of class <character> fuzz
      559 559  OK     `ignore_patterns` should be of class <character> fuzz
      560 560  OK     `ignore_patterns` should be of class <character> fuzz
      561 561  OK     `ignore_patterns` should be of class <character> fuzz
      562 562  OK     `ignore_patterns` should be of class <character> fuzz
      563 563  OK     `ignore_patterns` should be of class <character> fuzz
      564 564  OK     `ignore_patterns` should be of class <character> fuzz
      565 565  OK     `ignore_patterns` should be of class <character> fuzz
      566 566  OK     `ignore_patterns` should be of class <character> fuzz
      567 567  OK     `ignore_patterns` should be of class <character> fuzz
      568 568  OK     `ignore_patterns` should be of class <character> fuzz
      569 569  OK       `ignore_warnings` should be of class <logical> fuzz
      570 570  OK       `ignore_warnings` should be of class <logical> fuzz
      571 571  OK       `ignore_warnings` should be of class <logical> fuzz
      572 572  OK       `ignore_warnings` should be of class <logical> fuzz
      573 573  OK       `ignore_warnings` should be of class <logical> fuzz
      574 574  OK       `ignore_warnings` should be of class <logical> fuzz
      575 575  OK       `ignore_warnings` should be of class <logical> fuzz
      576 576  OK       `ignore_warnings` should be of class <logical> fuzz
      577 577  OK       `ignore_warnings` should be of class <logical> fuzz
      578 578  OK       `ignore_warnings` should be of class <logical> fuzz
      579 579  OK       `ignore_warnings` should be of class <logical> fuzz
      580 580  OK       `ignore_warnings` should be of class <logical> fuzz
      581 581  OK       `ignore_warnings` should be of class <logical> fuzz
      582 582  OK       `ignore_warnings` should be of class <logical> fuzz
      583 583  OK       `ignore_warnings` should be of class <logical> fuzz
      584 584  OK       `ignore_warnings` should be of class <logical> fuzz
      585 585  OK       `ignore_warnings` should be of class <logical> fuzz
      586 586  OK       `ignore_warnings` should be of class <logical> fuzz
      587 587  OK       `ignore_warnings` should be of class <logical> fuzz
      588 588  OK       `ignore_warnings` should be of class <logical> fuzz
      589 589  OK       `ignore_warnings` should be of class <logical> fuzz
      590 590  OK       `ignore_warnings` should be of class <logical> fuzz
      591 591  OK       `ignore_warnings` should be of class <logical> fuzz
      592 592  OK       `ignore_warnings` should be of class <logical> fuzz
      593 593  OK       `ignore_warnings` should be of class <logical> fuzz
      594 594  OK                                                      fuzz
      595 595  OK `ignore_warnings` should be a single <logical> value fuzz
      596 596  OK              `ignore_warnings` is an empty <logical> fuzz
      597 597  OK       `ignore_warnings` should be of class <logical> fuzz
      598 598  OK       `ignore_warnings` should be of class <logical> fuzz
      599 599  OK       `ignore_warnings` should be of class <logical> fuzz
      600 600  OK       `ignore_warnings` should be of class <logical> fuzz
      601 601  OK       `ignore_warnings` should be of class <logical> fuzz
      602 602  OK       `ignore_warnings` should be of class <logical> fuzz
      603 603  OK       `ignore_warnings` should be of class <logical> fuzz
      604 604  OK       `ignore_warnings` should be of class <logical> fuzz
      605 605  OK       `ignore_warnings` should be of class <logical> fuzz
      606 606  OK       `ignore_warnings` should be of class <logical> fuzz
      607 607  OK       `ignore_warnings` should be of class <logical> fuzz
      608 608  OK       `ignore_warnings` should be of class <logical> fuzz
      609 609  OK       `ignore_warnings` should be of class <logical> fuzz
      610 610  OK       `ignore_warnings` should be of class <logical> fuzz
      611 611  OK       `ignore_warnings` should be of class <logical> fuzz
      612 612  OK       `ignore_warnings` should be of class <logical> fuzz
      613 613  OK       `ignore_warnings` should be of class <logical> fuzz
      614 614  OK       `ignore_warnings` should be of class <logical> fuzz
      615 615  OK       `ignore_warnings` should be of class <logical> fuzz
      616 616  OK       `ignore_warnings` should be of class <logical> fuzz
      617 617  OK       `ignore_warnings` should be of class <logical> fuzz
      618 618  OK       `ignore_warnings` should be of class <logical> fuzz
      619 619  OK       `ignore_warnings` should be of class <logical> fuzz
      620 620  OK       `ignore_warnings` should be of class <logical> fuzz
      621 621  OK       `ignore_warnings` should be of class <logical> fuzz
      622 622  OK       `ignore_warnings` should be of class <logical> fuzz
      623 623  OK       `ignore_warnings` should be of class <logical> fuzz
      624 624  OK       `ignore_warnings` should be of class <logical> fuzz
      625 625  OK       `ignore_warnings` should be of class <logical> fuzz
      626 626  OK       `ignore_warnings` should be of class <logical> fuzz
      627 627  OK       `ignore_warnings` should be of class <logical> fuzz
      628 628  OK       `ignore_warnings` should be of class <logical> fuzz
      629 629  OK       `ignore_warnings` should be of class <logical> fuzz
      630 630  OK       `ignore_warnings` should be of class <logical> fuzz
      631 631  OK       `ignore_warnings` should be of class <logical> fuzz
      632 632  OK       `ignore_warnings` should be of class <logical> fuzz
      633 633  OK       `ignore_warnings` should be of class <logical> fuzz
      634 634  OK       `ignore_warnings` should be of class <logical> fuzz
      635 635  OK       `ignore_warnings` should be of class <logical> fuzz
      636 636  OK       `ignore_warnings` should be of class <logical> fuzz
      637 637  OK       `ignore_warnings` should be of class <logical> fuzz
      638 638  OK       `ignore_warnings` should be of class <logical> fuzz
      639 639  OK       `ignore_warnings` should be of class <logical> fuzz
      640 640  OK       `ignore_warnings` should be of class <logical> fuzz
      641 641  OK       `ignore_warnings` should be of class <logical> fuzz
      642 642  OK       `ignore_warnings` should be of class <logical> fuzz
      643 643  OK       `ignore_warnings` should be of class <logical> fuzz
      644 644  OK       `ignore_warnings` should be of class <logical> fuzz
      645 645  OK       `ignore_warnings` should be of class <logical> fuzz
      646 646  OK       `ignore_warnings` should be of class <logical> fuzz
      647 647  OK       `ignore_warnings` should be of class <logical> fuzz
      648 648  OK       `ignore_warnings` should be of class <logical> fuzz
      649 649  OK       `ignore_warnings` should be of class <logical> fuzz
      650 650  OK       `ignore_warnings` should be of class <logical> fuzz
      651 651  OK       `ignore_warnings` should be of class <logical> fuzz
      652 652  OK       `ignore_warnings` should be of class <logical> fuzz
      653 653  OK       `ignore_warnings` should be of class <logical> fuzz
      654 654  OK       `ignore_warnings` should be of class <logical> fuzz
      655 655  OK       `ignore_warnings` should be of class <logical> fuzz
      656 656  OK       `ignore_warnings` should be of class <logical> fuzz
      657 657  OK       `ignore_warnings` should be of class <logical> fuzz
      658 658  OK       `ignore_warnings` should be of class <logical> fuzz
      659 659  OK       `ignore_warnings` should be of class <logical> fuzz
      660 660  OK       `ignore_warnings` should be of class <logical> fuzz
      661 661  OK       `ignore_warnings` should be of class <logical> fuzz
      662 662  OK       `ignore_warnings` should be of class <logical> fuzz
      663 663  OK       `ignore_warnings` should be of class <logical> fuzz
      664 664  OK                                                      fuzz
      665 665  OK                       `daemons` should be at least 1 fuzz
      666 666  OK                       `daemons` should be at least 1 fuzz
      667 667  OK                                                      fuzz
      668 668  OK                       `daemons` should be at least 1 fuzz
      669 669  OK                                                      fuzz
      670 670  OK                                                      fuzz
      671 671  OK                       `daemons` should be at least 1 fuzz
      672 672  OK       `daemons` should be of class <integer/numeric> fuzz
      673 673  OK       `daemons` should be of class <integer/numeric> fuzz
      674 674  OK       `daemons` should be of class <integer/numeric> fuzz
      675 675  OK       `daemons` should be of class <integer/numeric> fuzz
      676 676  OK       `daemons` should be of class <integer/numeric> fuzz
      677 677  OK         `daemons` should be a single <numeric> value fuzz
      678 678  OK         `daemons` should be a single <numeric> value fuzz
      679 679  OK         `daemons` should be a single <numeric> value fuzz
      680 680  OK         `daemons` should be a single <numeric> value fuzz
      681 681  OK         `daemons` should be a single <numeric> value fuzz
      682 682  OK         `daemons` should be a single <numeric> value fuzz
      683 683  OK         `daemons` should be a single <numeric> value fuzz
      684 684  OK                       `daemons` should be at least 1 fuzz
      685 685  OK         `daemons` should be a single <integer> value fuzz
      686 686  OK         `daemons` should be a single <integer> value fuzz
      687 687  OK         `daemons` should be a single <integer> value fuzz
      688 688  OK                       `daemons` should be at least 1 fuzz
      689 689  OK       `daemons` should be of class <integer/numeric> fuzz
      690 690  OK       `daemons` should be of class <integer/numeric> fuzz
      691 691  OK       `daemons` should be of class <integer/numeric> fuzz
      692 692  OK       `daemons` should be of class <integer/numeric> fuzz
      693 693  OK       `daemons` should be of class <integer/numeric> fuzz
      694 694  OK       `daemons` should be of class <integer/numeric> fuzz
      695 695  OK       `daemons` should be of class <integer/numeric> fuzz
      696 696  OK       `daemons` should be of class <integer/numeric> fuzz
      697 697  OK       `daemons` should be of class <integer/numeric> fuzz
      698 698  OK       `daemons` should be of class <integer/numeric> fuzz
      699 699  OK       `daemons` should be of class <integer/numeric> fuzz
      700 700  OK       `daemons` should be of class <integer/numeric> fuzz
      701 701  OK       `daemons` should be of class <integer/numeric> fuzz
      702 702  OK       `daemons` should be of class <integer/numeric> fuzz
      703 703  OK       `daemons` should be of class <integer/numeric> fuzz
      704 704  OK       `daemons` should be of class <integer/numeric> fuzz
      705 705  OK       `daemons` should be of class <integer/numeric> fuzz
      706 706  OK       `daemons` should be of class <integer/numeric> fuzz
      707 707  OK       `daemons` should be of class <integer/numeric> fuzz
      708 708  OK       `daemons` should be of class <integer/numeric> fuzz
      709 709  OK       `daemons` should be of class <integer/numeric> fuzz
      710 710  OK       `daemons` should be of class <integer/numeric> fuzz
      711 711  OK       `daemons` should be of class <integer/numeric> fuzz
      712 712  OK       `daemons` should be of class <integer/numeric> fuzz
      713 713  OK       `daemons` should be of class <integer/numeric> fuzz
      714 714  OK       `daemons` should be of class <integer/numeric> fuzz
      715 715  OK       `daemons` should be of class <integer/numeric> fuzz
      716 716  OK       `daemons` should be of class <integer/numeric> fuzz
      717 717  OK       `daemons` should be of class <integer/numeric> fuzz
      718 718  OK       `daemons` should be of class <integer/numeric> fuzz
      719 719  OK       `daemons` should be of class <integer/numeric> fuzz
      720 720  OK       `daemons` should be of class <integer/numeric> fuzz
      721 721  OK       `daemons` should be of class <integer/numeric> fuzz
      722 722  OK       `daemons` should be of class <integer/numeric> fuzz
      723 723  OK       `daemons` should be of class <integer/numeric> fuzz
      724 724  OK       `daemons` should be of class <integer/numeric> fuzz
      725 725  OK       `daemons` should be of class <integer/numeric> fuzz
      726 726  OK       `daemons` should be of class <integer/numeric> fuzz
      727 727  OK       `daemons` should be of class <integer/numeric> fuzz
      728 728  OK       `daemons` should be of class <integer/numeric> fuzz
      729 729  OK       `daemons` should be of class <integer/numeric> fuzz
      730 730  OK       `daemons` should be of class <integer/numeric> fuzz
      731 731  OK       `daemons` should be of class <integer/numeric> fuzz
      732 732  OK       `daemons` should be of class <integer/numeric> fuzz
      733 733  OK       `daemons` should be of class <integer/numeric> fuzz
      734 734  OK       `daemons` should be of class <integer/numeric> fuzz
      735 735  OK       `daemons` should be of class <integer/numeric> fuzz
      736 736  OK       `daemons` should be of class <integer/numeric> fuzz
      737 737  OK       `daemons` should be of class <integer/numeric> fuzz
      738 738  OK       `daemons` should be of class <integer/numeric> fuzz
      739 739  OK       `daemons` should be of class <integer/numeric> fuzz
      740 740  OK       `daemons` should be of class <integer/numeric> fuzz
      741 741  OK       `daemons` should be of class <integer/numeric> fuzz
      742 742  OK       `daemons` should be of class <integer/numeric> fuzz
      743 743  OK       `daemons` should be of class <integer/numeric> fuzz
      744 744  OK       `daemons` should be of class <integer/numeric> fuzz
      745 745  OK       `daemons` should be of class <integer/numeric> fuzz
      746 746  OK       `daemons` should be of class <integer/numeric> fuzz
      747 747  OK       `daemons` should be of class <integer/numeric> fuzz
      748 748  OK       `daemons` should be of class <integer/numeric> fuzz
      749 749  OK       `daemons` should be of class <integer/numeric> fuzz
      750 750  OK       `daemons` should be of class <integer/numeric> fuzz
      751 751  OK       `daemons` should be of class <integer/numeric> fuzz
      752 752  OK       `daemons` should be of class <integer/numeric> fuzz
      753 753  OK       `daemons` should be of class <integer/numeric> fuzz
      754 754  OK       `daemons` should be of class <integer/numeric> fuzz
      755 755  OK       `daemons` should be of class <integer/numeric> fuzz
      756 756  OK       `daemons` should be of class <integer/numeric> fuzz
      757 757  OK       `daemons` should be of class <integer/numeric> fuzz
      758 758  OK       `daemons` should be of class <integer/numeric> fuzz
      759 759  OK                                                      fuzz
      760 760  OK                     `timeout` should be at least 0.5 fuzz
      761 761  OK                     `timeout` should be at least 0.5 fuzz
      762 762  OK                                                      fuzz
      763 763  OK                     `timeout` should be at least 0.5 fuzz
      764 764  OK                                                      fuzz
      765 765  OK                                                      fuzz
      766 766  OK                     `timeout` should be at least 0.5 fuzz
      767 767  OK       `timeout` should be of class <numeric/integer> fuzz
      768 768  OK       `timeout` should be of class <numeric/integer> fuzz
      769 769  OK       `timeout` should be of class <numeric/integer> fuzz
      770 770  OK       `timeout` should be of class <numeric/integer> fuzz
      771 771  OK       `timeout` should be of class <numeric/integer> fuzz
      772 772  OK         `timeout` should be a single <numeric> value fuzz
      773 773  OK         `timeout` should be a single <numeric> value fuzz
      774 774  OK         `timeout` should be a single <numeric> value fuzz
      775 775  OK         `timeout` should be a single <numeric> value fuzz
      776 776  OK         `timeout` should be a single <numeric> value fuzz
      777 777  OK         `timeout` should be a single <numeric> value fuzz
      778 778  OK         `timeout` should be a single <numeric> value fuzz
      779 779  OK                     `timeout` should be at least 0.5 fuzz
      780 780  OK         `timeout` should be a single <integer> value fuzz
      781 781  OK         `timeout` should be a single <integer> value fuzz
      782 782  OK         `timeout` should be a single <integer> value fuzz
      783 783  OK                     `timeout` should be at least 0.5 fuzz
      784 784  OK       `timeout` should be of class <numeric/integer> fuzz
      785 785  OK       `timeout` should be of class <numeric/integer> fuzz
      786 786  OK       `timeout` should be of class <numeric/integer> fuzz
      787 787  OK       `timeout` should be of class <numeric/integer> fuzz
      788 788  OK       `timeout` should be of class <numeric/integer> fuzz
      789 789  OK       `timeout` should be of class <numeric/integer> fuzz
      790 790  OK       `timeout` should be of class <numeric/integer> fuzz
      791 791  OK       `timeout` should be of class <numeric/integer> fuzz
      792 792  OK       `timeout` should be of class <numeric/integer> fuzz
      793 793  OK       `timeout` should be of class <numeric/integer> fuzz
      794 794  OK       `timeout` should be of class <numeric/integer> fuzz
      795 795  OK       `timeout` should be of class <numeric/integer> fuzz
      796 796  OK       `timeout` should be of class <numeric/integer> fuzz
      797 797  OK       `timeout` should be of class <numeric/integer> fuzz
      798 798  OK       `timeout` should be of class <numeric/integer> fuzz
      799 799  OK       `timeout` should be of class <numeric/integer> fuzz
      800 800  OK       `timeout` should be of class <numeric/integer> fuzz
      801 801  OK       `timeout` should be of class <numeric/integer> fuzz
      802 802  OK       `timeout` should be of class <numeric/integer> fuzz
      803 803  OK       `timeout` should be of class <numeric/integer> fuzz
      804 804  OK       `timeout` should be of class <numeric/integer> fuzz
      805 805  OK       `timeout` should be of class <numeric/integer> fuzz
      806 806  OK       `timeout` should be of class <numeric/integer> fuzz
      807 807  OK       `timeout` should be of class <numeric/integer> fuzz
      808 808  OK       `timeout` should be of class <numeric/integer> fuzz
      809 809  OK       `timeout` should be of class <numeric/integer> fuzz
      810 810  OK       `timeout` should be of class <numeric/integer> fuzz
      811 811  OK       `timeout` should be of class <numeric/integer> fuzz
      812 812  OK       `timeout` should be of class <numeric/integer> fuzz
      813 813  OK       `timeout` should be of class <numeric/integer> fuzz
      814 814  OK       `timeout` should be of class <numeric/integer> fuzz
      815 815  OK       `timeout` should be of class <numeric/integer> fuzz
      816 816  OK       `timeout` should be of class <numeric/integer> fuzz
      817 817  OK       `timeout` should be of class <numeric/integer> fuzz
      818 818  OK       `timeout` should be of class <numeric/integer> fuzz
      819 819  OK       `timeout` should be of class <numeric/integer> fuzz
      820 820  OK       `timeout` should be of class <numeric/integer> fuzz
      821 821  OK       `timeout` should be of class <numeric/integer> fuzz
      822 822  OK       `timeout` should be of class <numeric/integer> fuzz
      823 823  OK       `timeout` should be of class <numeric/integer> fuzz
      824 824  OK       `timeout` should be of class <numeric/integer> fuzz
      825 825  OK       `timeout` should be of class <numeric/integer> fuzz
      826 826  OK       `timeout` should be of class <numeric/integer> fuzz
      827 827  OK       `timeout` should be of class <numeric/integer> fuzz
      828 828  OK       `timeout` should be of class <numeric/integer> fuzz
      829 829  OK       `timeout` should be of class <numeric/integer> fuzz
      830 830  OK       `timeout` should be of class <numeric/integer> fuzz
      831 831  OK       `timeout` should be of class <numeric/integer> fuzz
      832 832  OK       `timeout` should be of class <numeric/integer> fuzz
      833 833  OK       `timeout` should be of class <numeric/integer> fuzz
      834 834  OK       `timeout` should be of class <numeric/integer> fuzz
      835 835  OK       `timeout` should be of class <numeric/integer> fuzz
      836 836  OK       `timeout` should be of class <numeric/integer> fuzz
      837 837  OK       `timeout` should be of class <numeric/integer> fuzz
      838 838  OK       `timeout` should be of class <numeric/integer> fuzz
      839 839  OK       `timeout` should be of class <numeric/integer> fuzz
      840 840  OK       `timeout` should be of class <numeric/integer> fuzz
      841 841  OK       `timeout` should be of class <numeric/integer> fuzz
      842 842  OK       `timeout` should be of class <numeric/integer> fuzz
      843 843  OK       `timeout` should be of class <numeric/integer> fuzz
      844 844  OK       `timeout` should be of class <numeric/integer> fuzz
      845 845  OK       `timeout` should be of class <numeric/integer> fuzz
      846 846  OK       `timeout` should be of class <numeric/integer> fuzz
      847 847  OK       `timeout` should be of class <numeric/integer> fuzz
      848 848  OK       `timeout` should be of class <numeric/integer> fuzz
      849 849  OK       `timeout` should be of class <numeric/integer> fuzz
      850 850  OK       `timeout` should be of class <numeric/integer> fuzz
      851 851  OK       `timeout` should be of class <numeric/integer> fuzz
      852 852  OK       `timeout` should be of class <numeric/integer> fuzz
      853 853  OK       `timeout` should be of class <numeric/integer> fuzz
                                                                                                                                                                                                                                     what
      1                                                                         funs = 3L, what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      2                                                                         funs = 0L, what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      3                                                                        funs = -4L, what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      4                                                       funs = .Machine$integer.max, what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      5                                                       funs = .Machine$double.xmin, what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      6                                                       funs = .Machine$double.xmax, what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      7                                                                        funs = 1.2, what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      8                                                                       funs = -2.5, what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      9                                                                       funs = 0+6i, what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      10                                                                       funs = NaN, what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      11                                                                       funs = Inf, what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      12                                                                      funs = -Inf, what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      13                                                                      funs = NULL, what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      14                                               funs = c(94.3, 1038345.2, 18.2345), what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      15                                          funs = c(1.309605, 0.585381, -0.461072), what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      16                                         funs = c(0.9876, 0.5432, 0.2345, 0.3456), what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      17                                                      funs = c(-1, 0, NaN, 10000), what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      18                                                        funs = c(Inf, -0.5, 1234), what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      19                                                                   funs = c(0, 0), what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      20                                                                  funs = c(0, NA), what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      21                                                                 funs = numeric(), what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      22                                                  funs = c(2L, 5L, 123456L, 100L), what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      23                                                                      funs = -1:3, what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      24                                                                 funs = c(0L, NA), what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      25                                                                 funs = integer(), what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      26                                                                      funs = TRUE, what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      27                                                        funs = c(TRUE, NA, FALSE), what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      28                                                                 funs = logical(), what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      29                                                                   funs = LETTERS, what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      30                                                                        funs = "", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      31                                                                  funs = "a test", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      32                                                            funs = c("", "B", ""), what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      33                                                      funs = c("A", NA, "7", "+"), what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      34                                                               funs = character(), what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      35                                                                funs = factor(""), what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      36                                              funs = factor(c("A", NA, "7", "+")), what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      37                       funs = factor(c("a", NA, "c", "d"), levels = letters[1:4]), what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      38                                                 funs = factor(c(NA, NA, NA, NA)), what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      39                                funs = factor(1:4, levels = 10:1, ordered = TRUE), what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      40       funs = factor(c("d", NA, "a", "b"), levels = letters[1:4], ordered = TRUE), what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      41                                                                  funs = factor(), what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      42                                                     funs = datasets::iris[, 3:5], what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      43                                                        funs = data.frame(a = NA), what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      44                                                   funs = data.frame(a = letters), what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      45                                              funs = data.frame(a = 1:10, b = NA), what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      46                                             funs = data.frame(a = -1:1, b = Inf), what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      47         funs = data.frame(a = c(12, -2.1, 7), b = c(1.34, 2.28, 0), c = (-1:1 + , what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      48                                                       funs = datasets::iris[0, ], what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      49                                                       funs = datasets::iris[, 0], what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      50                                         funs = datasets::iris[1, , drop = FALSE], what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      51                                         funs = datasets::iris[, 1, drop = FALSE], what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      52                                                              funs = data.frame(), what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      53                                                           funs = matrix(0, 0, 0), what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      54                                                           funs = matrix(1, 1, 0), what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      55                                                           funs = matrix(1, 0, 1), what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      56            funs = matrix(c(1.995874, 1.225707, -0.565287, -2.120309, 0.236326), , what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      57             funs = matrix(c(-1.00577, 0.589365, 0.955131, -0.972982, 1.058721), , what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      58                                           funs = matrix(c(1:7, NA, -1:-4), 3, 4), what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      59                                                funs = matrix(c(-1:6, Inf), 3, 3), what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      60                                                    funs = matrix(letters, 13, 2), what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      61                                                                  funs = matrix(), what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      62                                                      funs = array(0, c(0, 0, 0)), what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      63                                                      funs = array(1, c(1, 1, 0)), what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      64                                                      funs = array(1, c(0, 1, 0)), what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      65             funs = array(c(0.7235, -0.19754, 1.67188, -0.62163, -0.84149), c(1, , what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      66            funs = array(c(-0.16868, 0.86973, -0.46976, 0.32157, -0.43558), c(1, , what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      67            funs = array(c(-0.57441, -1.9585, -0.24757, -0.2467, -2.04617), c(5, , what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      68                                             funs = array(c(1:7, NA), c(2, 4, 1)), what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      69                                         funs = array(c(1:5, NA, NA), c(2, 4, 2)), what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      70                                      funs = array(c(1:3, Inf, 12.5), c(2, 4, 2)), what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      71                                                                   funs = array(), what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      72                                                     funs = as.Date("2045-12-25"), what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      73                                                               funs = as.Date(NA), what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      74                                                             funs = as.Date(NULL), what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      75                                                                funs = as.Date(0), what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      76                                 funs = as.Date(c("2025-01-01", NA, "930-12-31")), what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      77                                                                funs = Sys.Date(), what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      78              funs = as.POSIXct(c(1048584960, 47764160, 1256110400, 10562745600)), what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      79                                                            funs = as.POSIXct(NA), what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      80                                                          funs = as.POSIXct(NULL), what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      81                                         funs = as.POSIXct("2065-01-01 00:00:00"), what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      82                                      funs = as.POSIXct(0, origin = "1970-01-01"), what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      83                                                    funs = as.POSIXlt(Sys.time()), what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      84                                                                funs = Sys.time(), what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      85                                                            funs = charToRaw("0"), what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      86                                                  funs = charToRaw(NA_character_), what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      87                                                          funs = charToRaw("abc"), what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      88                                                                     funs = raw(), what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      89                                                                  funs = NA_real_, what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      90                                                               funs = NA_integer_, what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      91                                                             funs = NA_character_, what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      92                                                                        funs = NA, what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      93                                                                   funs = list(3), what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      94                                         funs = list(a = c(1, 2, 3), b = letters), what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      95                                                                    funs = list(), what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      96                                                                            funs = "list", what = 3L, args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      97                                                                            funs = "list", what = 0L, args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      98                                                                           funs = "list", what = -4L, args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      99                                                          funs = "list", what = .Machine$integer.max, args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      100                                                         funs = "list", what = .Machine$double.xmin, args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      101                                                         funs = "list", what = .Machine$double.xmax, args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      102                                                                          funs = "list", what = 1.2, args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      103                                                                         funs = "list", what = -2.5, args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      104                                                                         funs = "list", what = 0+6i, args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      105                                                                          funs = "list", what = NaN, args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      106                                                                          funs = "list", what = Inf, args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      107                                                                         funs = "list", what = -Inf, args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      108                                                                         funs = "list", what = NULL, args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      109                                                  funs = "list", what = c(94.3, 1038345.2, 18.2345), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      110                                             funs = "list", what = c(1.309605, 0.585381, -0.461072), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      111                                            funs = "list", what = c(0.9876, 0.5432, 0.2345, 0.3456), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      112                                                         funs = "list", what = c(-1, 0, NaN, 10000), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      113                                                           funs = "list", what = c(Inf, -0.5, 1234), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      114                                                                      funs = "list", what = c(0, 0), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      115                                                                     funs = "list", what = c(0, NA), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      116                                                                    funs = "list", what = numeric(), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      117                                                     funs = "list", what = c(2L, 5L, 123456L, 100L), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      118                                                                         funs = "list", what = -1:3, args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      119                                                                    funs = "list", what = c(0L, NA), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      120                                                                    funs = "list", what = integer(), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      121                                                                         funs = "list", what = TRUE, args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      122                                                           funs = "list", what = c(TRUE, NA, FALSE), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      123                                                                    funs = "list", what = logical(), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      124                                                                      funs = "list", what = LETTERS, args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      125                                                                           funs = "list", what = "", args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      126                                                                     funs = "list", what = "a test", args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      127                                                               funs = "list", what = c("", "B", ""), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      128                                                         funs = "list", what = c("A", NA, "7", "+"), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      129                                                                  funs = "list", what = character(), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      130                                                                   funs = "list", what = factor(""), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      131                                                 funs = "list", what = factor(c("A", NA, "7", "+")), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      132                          funs = "list", what = factor(c("a", NA, "c", "d"), levels = letters[1:4]), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      133                                                    funs = "list", what = factor(c(NA, NA, NA, NA)), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      134                                   funs = "list", what = factor(1:4, levels = 10:1, ordered = TRUE), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      135          funs = "list", what = factor(c("d", NA, "a", "b"), levels = letters[1:4], ordered = TRUE), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      136                                                                     funs = "list", what = factor(), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      137                                                        funs = "list", what = datasets::iris[, 3:5], args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      138                                                           funs = "list", what = data.frame(a = NA), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      139                                                      funs = "list", what = data.frame(a = letters), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      140                                                 funs = "list", what = data.frame(a = 1:10, b = NA), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      141                                                funs = "list", what = data.frame(a = -1:1, b = Inf), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      142            funs = "list", what = data.frame(a = c(12, -2.1, 7), b = c(1.34, 2.28, 0), c = (-1:1 + , args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      143                                                          funs = "list", what = datasets::iris[0, ], args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      144                                                          funs = "list", what = datasets::iris[, 0], args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      145                                            funs = "list", what = datasets::iris[1, , drop = FALSE], args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      146                                            funs = "list", what = datasets::iris[, 1, drop = FALSE], args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      147                                                                 funs = "list", what = data.frame(), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      148                                                              funs = "list", what = matrix(0, 0, 0), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      149                                                              funs = "list", what = matrix(1, 1, 0), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      150                                                              funs = "list", what = matrix(1, 0, 1), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      151               funs = "list", what = matrix(c(1.995874, 1.225707, -0.565287, -2.120309, 0.236326), , args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      152                funs = "list", what = matrix(c(-1.00577, 0.589365, 0.955131, -0.972982, 1.058721), , args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      153                                              funs = "list", what = matrix(c(1:7, NA, -1:-4), 3, 4), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      154                                                   funs = "list", what = matrix(c(-1:6, Inf), 3, 3), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      155                                                       funs = "list", what = matrix(letters, 13, 2), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      156                                                                     funs = "list", what = matrix(), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      157                                                         funs = "list", what = array(0, c(0, 0, 0)), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      158                                                         funs = "list", what = array(1, c(1, 1, 0)), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      159                                                         funs = "list", what = array(1, c(0, 1, 0)), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      160                funs = "list", what = array(c(0.7235, -0.19754, 1.67188, -0.62163, -0.84149), c(1, , args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      161               funs = "list", what = array(c(-0.16868, 0.86973, -0.46976, 0.32157, -0.43558), c(1, , args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      162               funs = "list", what = array(c(-0.57441, -1.9585, -0.24757, -0.2467, -2.04617), c(5, , args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      163                                                funs = "list", what = array(c(1:7, NA), c(2, 4, 1)), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      164                                            funs = "list", what = array(c(1:5, NA, NA), c(2, 4, 2)), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      165                                         funs = "list", what = array(c(1:3, Inf, 12.5), c(2, 4, 2)), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      166                                                                      funs = "list", what = array(), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      167                                                        funs = "list", what = as.Date("2045-12-25"), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      168                                                                  funs = "list", what = as.Date(NA), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      169                                                                funs = "list", what = as.Date(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      170                                                                   funs = "list", what = as.Date(0), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      171                                    funs = "list", what = as.Date(c("2025-01-01", NA, "930-12-31")), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      172                                                                   funs = "list", what = Sys.Date(), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      173                 funs = "list", what = as.POSIXct(c(1048584960, 47764160, 1256110400, 10562745600)), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      174                                                               funs = "list", what = as.POSIXct(NA), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      175                                                             funs = "list", what = as.POSIXct(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      176                                            funs = "list", what = as.POSIXct("2065-01-01 00:00:00"), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      177                                         funs = "list", what = as.POSIXct(0, origin = "1970-01-01"), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      178                                                       funs = "list", what = as.POSIXlt(Sys.time()), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      179                                                                   funs = "list", what = Sys.time(), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      180                                                               funs = "list", what = charToRaw("0"), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      181                                                     funs = "list", what = charToRaw(NA_character_), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      182                                                             funs = "list", what = charToRaw("abc"), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      183                                                                        funs = "list", what = raw(), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      184                                                                     funs = "list", what = NA_real_, args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      185                                                                  funs = "list", what = NA_integer_, args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      186                                                                funs = "list", what = NA_character_, args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      187                                                                           funs = "list", what = NA, args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      188                                                                      funs = "list", what = list(3), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      189                                            funs = "list", what = list(a = c(1, 2, 3), b = letters), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      190                                                                       funs = "list", what = list(), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      191                                                                     funs = "list", what = list(NULL), args = 3L, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      192                                                                     funs = "list", what = list(NULL), args = 0L, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      193                                                                    funs = "list", what = list(NULL), args = -4L, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      194                                                   funs = "list", what = list(NULL), args = .Machine$integer.max, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      195                                                   funs = "list", what = list(NULL), args = .Machine$double.xmin, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      196                                                   funs = "list", what = list(NULL), args = .Machine$double.xmax, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      197                                                                    funs = "list", what = list(NULL), args = 1.2, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      198                                                                   funs = "list", what = list(NULL), args = -2.5, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      199                                                                   funs = "list", what = list(NULL), args = 0+6i, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      200                                                                    funs = "list", what = list(NULL), args = NaN, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      201                                                                    funs = "list", what = list(NULL), args = Inf, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      202                                                                   funs = "list", what = list(NULL), args = -Inf, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      203                                                                   funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      204                                            funs = "list", what = list(NULL), args = c(94.3, 1038345.2, 18.2345), package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      205                                       funs = "list", what = list(NULL), args = c(1.309605, 0.585381, -0.461072), package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      206                                      funs = "list", what = list(NULL), args = c(0.9876, 0.5432, 0.2345, 0.3456), package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      207                                                   funs = "list", what = list(NULL), args = c(-1, 0, NaN, 10000), package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      208                                                     funs = "list", what = list(NULL), args = c(Inf, -0.5, 1234), package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      209                                                                funs = "list", what = list(NULL), args = c(0, 0), package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      210                                                               funs = "list", what = list(NULL), args = c(0, NA), package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      211                                                              funs = "list", what = list(NULL), args = numeric(), package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      212                                               funs = "list", what = list(NULL), args = c(2L, 5L, 123456L, 100L), package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      213                                                                   funs = "list", what = list(NULL), args = -1:3, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      214                                                              funs = "list", what = list(NULL), args = c(0L, NA), package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      215                                                              funs = "list", what = list(NULL), args = integer(), package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      216                                                                   funs = "list", what = list(NULL), args = TRUE, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      217                                                     funs = "list", what = list(NULL), args = c(TRUE, NA, FALSE), package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      218                                                              funs = "list", what = list(NULL), args = logical(), package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      219                                                                funs = "list", what = list(NULL), args = LETTERS, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      220                                                                     funs = "list", what = list(NULL), args = "", package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      221                                                               funs = "list", what = list(NULL), args = "a test", package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      222                                                         funs = "list", what = list(NULL), args = c("", "B", ""), package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      223                                                   funs = "list", what = list(NULL), args = c("A", NA, "7", "+"), package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      224                                                            funs = "list", what = list(NULL), args = character(), package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      225                                                             funs = "list", what = list(NULL), args = factor(""), package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      226                                           funs = "list", what = list(NULL), args = factor(c("A", NA, "7", "+")), package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      227                    funs = "list", what = list(NULL), args = factor(c("a", NA, "c", "d"), levels = letters[1:4]), package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      228                                              funs = "list", what = list(NULL), args = factor(c(NA, NA, NA, NA)), package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      229                             funs = "list", what = list(NULL), args = factor(1:4, levels = 10:1, ordered = TRUE), package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      230    funs = "list", what = list(NULL), args = factor(c("d", NA, "a", "b"), levels = letters[1:4], ordered = TRUE), package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      231                                                               funs = "list", what = list(NULL), args = factor(), package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      232                                                  funs = "list", what = list(NULL), args = datasets::iris[, 3:5], package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      233                                                     funs = "list", what = list(NULL), args = data.frame(a = NA), package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      234                                                funs = "list", what = list(NULL), args = data.frame(a = letters), package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      235                                           funs = "list", what = list(NULL), args = data.frame(a = 1:10, b = NA), package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      236                                          funs = "list", what = list(NULL), args = data.frame(a = -1:1, b = Inf), package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      237      funs = "list", what = list(NULL), args = data.frame(a = c(12, -2.1, 7), b = c(1.34, 2.28, 0), c = (-1:1 + , package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      238                                                    funs = "list", what = list(NULL), args = datasets::iris[0, ], package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      239                                                    funs = "list", what = list(NULL), args = datasets::iris[, 0], package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      240                                      funs = "list", what = list(NULL), args = datasets::iris[1, , drop = FALSE], package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      241                                      funs = "list", what = list(NULL), args = datasets::iris[, 1, drop = FALSE], package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      242                                                           funs = "list", what = list(NULL), args = data.frame(), package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      243                                                        funs = "list", what = list(NULL), args = matrix(0, 0, 0), package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      244                                                        funs = "list", what = list(NULL), args = matrix(1, 1, 0), package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      245                                                        funs = "list", what = list(NULL), args = matrix(1, 0, 1), package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      246         funs = "list", what = list(NULL), args = matrix(c(1.995874, 1.225707, -0.565287, -2.120309, 0.236326), , package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      247          funs = "list", what = list(NULL), args = matrix(c(-1.00577, 0.589365, 0.955131, -0.972982, 1.058721), , package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      248                                        funs = "list", what = list(NULL), args = matrix(c(1:7, NA, -1:-4), 3, 4), package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      249                                             funs = "list", what = list(NULL), args = matrix(c(-1:6, Inf), 3, 3), package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      250                                                 funs = "list", what = list(NULL), args = matrix(letters, 13, 2), package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      251                                                               funs = "list", what = list(NULL), args = matrix(), package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      252                                                   funs = "list", what = list(NULL), args = array(0, c(0, 0, 0)), package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      253                                                   funs = "list", what = list(NULL), args = array(1, c(1, 1, 0)), package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      254                                                   funs = "list", what = list(NULL), args = array(1, c(0, 1, 0)), package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      255          funs = "list", what = list(NULL), args = array(c(0.7235, -0.19754, 1.67188, -0.62163, -0.84149), c(1, , package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      256         funs = "list", what = list(NULL), args = array(c(-0.16868, 0.86973, -0.46976, 0.32157, -0.43558), c(1, , package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      257         funs = "list", what = list(NULL), args = array(c(-0.57441, -1.9585, -0.24757, -0.2467, -2.04617), c(5, , package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      258                                          funs = "list", what = list(NULL), args = array(c(1:7, NA), c(2, 4, 1)), package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      259                                      funs = "list", what = list(NULL), args = array(c(1:5, NA, NA), c(2, 4, 2)), package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      260                                   funs = "list", what = list(NULL), args = array(c(1:3, Inf, 12.5), c(2, 4, 2)), package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      261                                                                funs = "list", what = list(NULL), args = array(), package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      262                                                  funs = "list", what = list(NULL), args = as.Date("2045-12-25"), package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      263                                                            funs = "list", what = list(NULL), args = as.Date(NA), package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      264                                                          funs = "list", what = list(NULL), args = as.Date(NULL), package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      265                                                             funs = "list", what = list(NULL), args = as.Date(0), package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      266                              funs = "list", what = list(NULL), args = as.Date(c("2025-01-01", NA, "930-12-31")), package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      267                                                             funs = "list", what = list(NULL), args = Sys.Date(), package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      268           funs = "list", what = list(NULL), args = as.POSIXct(c(1048584960, 47764160, 1256110400, 10562745600)), package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      269                                                         funs = "list", what = list(NULL), args = as.POSIXct(NA), package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      270                                                       funs = "list", what = list(NULL), args = as.POSIXct(NULL), package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      271                                      funs = "list", what = list(NULL), args = as.POSIXct("2065-01-01 00:00:00"), package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      272                                   funs = "list", what = list(NULL), args = as.POSIXct(0, origin = "1970-01-01"), package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      273                                                 funs = "list", what = list(NULL), args = as.POSIXlt(Sys.time()), package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      274                                                             funs = "list", what = list(NULL), args = Sys.time(), package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      275                                                         funs = "list", what = list(NULL), args = charToRaw("0"), package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      276                                               funs = "list", what = list(NULL), args = charToRaw(NA_character_), package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      277                                                       funs = "list", what = list(NULL), args = charToRaw("abc"), package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      278                                                                  funs = "list", what = list(NULL), args = raw(), package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      279                                                               funs = "list", what = list(NULL), args = NA_real_, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      280                                                            funs = "list", what = list(NULL), args = NA_integer_, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      281                                                          funs = "list", what = list(NULL), args = NA_character_, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      282                                                                     funs = "list", what = list(NULL), args = NA, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      283                                                                funs = "list", what = list(NULL), args = list(3), package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      284                                      funs = "list", what = list(NULL), args = list(a = c(1, 2, 3), b = letters), package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      285                                                                 funs = "list", what = list(NULL), args = list(), package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      286                                                                     funs = "list", what = list(NULL), args = NULL, package = 3L, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      287                                                                     funs = "list", what = list(NULL), args = NULL, package = 0L, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      288                                                                    funs = "list", what = list(NULL), args = NULL, package = -4L, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      289                                                   funs = "list", what = list(NULL), args = NULL, package = .Machine$integer.max, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      290                                                   funs = "list", what = list(NULL), args = NULL, package = .Machine$double.xmin, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      291                                                   funs = "list", what = list(NULL), args = NULL, package = .Machine$double.xmax, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      292                                                                    funs = "list", what = list(NULL), args = NULL, package = 1.2, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      293                                                                   funs = "list", what = list(NULL), args = NULL, package = -2.5, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      294                                                                   funs = "list", what = list(NULL), args = NULL, package = 0+6i, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      295                                                                    funs = "list", what = list(NULL), args = NULL, package = NaN, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      296                                                                    funs = "list", what = list(NULL), args = NULL, package = Inf, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      297                                                                   funs = "list", what = list(NULL), args = NULL, package = -Inf, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      298                                            funs = "list", what = list(NULL), args = NULL, package = c(94.3, 1038345.2, 18.2345), listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      299                                       funs = "list", what = list(NULL), args = NULL, package = c(1.309605, 0.585381, -0.461072), listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      300                                      funs = "list", what = list(NULL), args = NULL, package = c(0.9876, 0.5432, 0.2345, 0.3456), listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      301                                                   funs = "list", what = list(NULL), args = NULL, package = c(-1, 0, NaN, 10000), listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      302                                                     funs = "list", what = list(NULL), args = NULL, package = c(Inf, -0.5, 1234), listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      303                                                                funs = "list", what = list(NULL), args = NULL, package = c(0, 0), listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      304                                                               funs = "list", what = list(NULL), args = NULL, package = c(0, NA), listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      305                                                              funs = "list", what = list(NULL), args = NULL, package = numeric(), listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      306                                               funs = "list", what = list(NULL), args = NULL, package = c(2L, 5L, 123456L, 100L), listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      307                                                                   funs = "list", what = list(NULL), args = NULL, package = -1:3, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      308                                                              funs = "list", what = list(NULL), args = NULL, package = c(0L, NA), listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      309                                                              funs = "list", what = list(NULL), args = NULL, package = integer(), listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      310                                                                   funs = "list", what = list(NULL), args = NULL, package = TRUE, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      311                                                     funs = "list", what = list(NULL), args = NULL, package = c(TRUE, NA, FALSE), listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      312                                                              funs = "list", what = list(NULL), args = NULL, package = logical(), listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      313                                                                funs = "list", what = list(NULL), args = NULL, package = LETTERS, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      314                                                                     funs = "list", what = list(NULL), args = NULL, package = "", listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      315                                                               funs = "list", what = list(NULL), args = NULL, package = "a test", listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      316                                                         funs = "list", what = list(NULL), args = NULL, package = c("", "B", ""), listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      317                                                   funs = "list", what = list(NULL), args = NULL, package = c("A", NA, "7", "+"), listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      318                                                            funs = "list", what = list(NULL), args = NULL, package = character(), listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      319                                                             funs = "list", what = list(NULL), args = NULL, package = factor(""), listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      320                                           funs = "list", what = list(NULL), args = NULL, package = factor(c("A", NA, "7", "+")), listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      321                    funs = "list", what = list(NULL), args = NULL, package = factor(c("a", NA, "c", "d"), levels = letters[1:4]), listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      322                                              funs = "list", what = list(NULL), args = NULL, package = factor(c(NA, NA, NA, NA)), listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      323                             funs = "list", what = list(NULL), args = NULL, package = factor(1:4, levels = 10:1, ordered = TRUE), listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      324    funs = "list", what = list(NULL), args = NULL, package = factor(c("d", NA, "a", "b"), levels = letters[1:4], ordered = TRUE), listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      325                                                               funs = "list", what = list(NULL), args = NULL, package = factor(), listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      326                                                  funs = "list", what = list(NULL), args = NULL, package = datasets::iris[, 3:5], listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      327                                                     funs = "list", what = list(NULL), args = NULL, package = data.frame(a = NA), listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      328                                                funs = "list", what = list(NULL), args = NULL, package = data.frame(a = letters), listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      329                                           funs = "list", what = list(NULL), args = NULL, package = data.frame(a = 1:10, b = NA), listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      330                                          funs = "list", what = list(NULL), args = NULL, package = data.frame(a = -1:1, b = Inf), listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      331      funs = "list", what = list(NULL), args = NULL, package = data.frame(a = c(12, -2.1, 7), b = c(1.34, 2.28, 0), c = (-1:1 + , listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      332                                                    funs = "list", what = list(NULL), args = NULL, package = datasets::iris[0, ], listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      333                                                    funs = "list", what = list(NULL), args = NULL, package = datasets::iris[, 0], listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      334                                      funs = "list", what = list(NULL), args = NULL, package = datasets::iris[1, , drop = FALSE], listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      335                                      funs = "list", what = list(NULL), args = NULL, package = datasets::iris[, 1, drop = FALSE], listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      336                                                           funs = "list", what = list(NULL), args = NULL, package = data.frame(), listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      337                                                        funs = "list", what = list(NULL), args = NULL, package = matrix(0, 0, 0), listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      338                                                        funs = "list", what = list(NULL), args = NULL, package = matrix(1, 1, 0), listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      339                                                        funs = "list", what = list(NULL), args = NULL, package = matrix(1, 0, 1), listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      340         funs = "list", what = list(NULL), args = NULL, package = matrix(c(1.995874, 1.225707, -0.565287, -2.120309, 0.236326), , listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      341          funs = "list", what = list(NULL), args = NULL, package = matrix(c(-1.00577, 0.589365, 0.955131, -0.972982, 1.058721), , listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      342                                        funs = "list", what = list(NULL), args = NULL, package = matrix(c(1:7, NA, -1:-4), 3, 4), listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      343                                             funs = "list", what = list(NULL), args = NULL, package = matrix(c(-1:6, Inf), 3, 3), listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      344                                                 funs = "list", what = list(NULL), args = NULL, package = matrix(letters, 13, 2), listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      345                                                               funs = "list", what = list(NULL), args = NULL, package = matrix(), listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      346                                                   funs = "list", what = list(NULL), args = NULL, package = array(0, c(0, 0, 0)), listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      347                                                   funs = "list", what = list(NULL), args = NULL, package = array(1, c(1, 1, 0)), listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      348                                                   funs = "list", what = list(NULL), args = NULL, package = array(1, c(0, 1, 0)), listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      349          funs = "list", what = list(NULL), args = NULL, package = array(c(0.7235, -0.19754, 1.67188, -0.62163, -0.84149), c(1, , listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      350         funs = "list", what = list(NULL), args = NULL, package = array(c(-0.16868, 0.86973, -0.46976, 0.32157, -0.43558), c(1, , listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      351         funs = "list", what = list(NULL), args = NULL, package = array(c(-0.57441, -1.9585, -0.24757, -0.2467, -2.04617), c(5, , listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      352                                          funs = "list", what = list(NULL), args = NULL, package = array(c(1:7, NA), c(2, 4, 1)), listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      353                                      funs = "list", what = list(NULL), args = NULL, package = array(c(1:5, NA, NA), c(2, 4, 2)), listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      354                                   funs = "list", what = list(NULL), args = NULL, package = array(c(1:3, Inf, 12.5), c(2, 4, 2)), listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      355                                                                funs = "list", what = list(NULL), args = NULL, package = array(), listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      356                                                  funs = "list", what = list(NULL), args = NULL, package = as.Date("2045-12-25"), listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      357                                                            funs = "list", what = list(NULL), args = NULL, package = as.Date(NA), listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      358                                                          funs = "list", what = list(NULL), args = NULL, package = as.Date(NULL), listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      359                                                             funs = "list", what = list(NULL), args = NULL, package = as.Date(0), listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      360                              funs = "list", what = list(NULL), args = NULL, package = as.Date(c("2025-01-01", NA, "930-12-31")), listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      361                                                             funs = "list", what = list(NULL), args = NULL, package = Sys.Date(), listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      362           funs = "list", what = list(NULL), args = NULL, package = as.POSIXct(c(1048584960, 47764160, 1256110400, 10562745600)), listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      363                                                         funs = "list", what = list(NULL), args = NULL, package = as.POSIXct(NA), listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      364                                                       funs = "list", what = list(NULL), args = NULL, package = as.POSIXct(NULL), listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      365                                      funs = "list", what = list(NULL), args = NULL, package = as.POSIXct("2065-01-01 00:00:00"), listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      366                                   funs = "list", what = list(NULL), args = NULL, package = as.POSIXct(0, origin = "1970-01-01"), listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      367                                                 funs = "list", what = list(NULL), args = NULL, package = as.POSIXlt(Sys.time()), listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      368                                                             funs = "list", what = list(NULL), args = NULL, package = Sys.time(), listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      369                                                         funs = "list", what = list(NULL), args = NULL, package = charToRaw("0"), listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      370                                               funs = "list", what = list(NULL), args = NULL, package = charToRaw(NA_character_), listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      371                                                       funs = "list", what = list(NULL), args = NULL, package = charToRaw("abc"), listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      372                                                                  funs = "list", what = list(NULL), args = NULL, package = raw(), listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      373                                                               funs = "list", what = list(NULL), args = NULL, package = NA_real_, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      374                                                            funs = "list", what = list(NULL), args = NULL, package = NA_integer_, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      375                                                          funs = "list", what = list(NULL), args = NULL, package = NA_character_, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      376                                                                     funs = "list", what = list(NULL), args = NULL, package = NA, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      377                                                                funs = "list", what = list(NULL), args = NULL, package = list(3), listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      378                                      funs = "list", what = list(NULL), args = NULL, package = list(a = c(1, 2, 3), b = letters), listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      379                                                                 funs = "list", what = list(NULL), args = NULL, package = list(), listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      380                                                                      funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = 3L, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      381                                                                      funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = 0L, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      382                                                                     funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = -4L, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      383                                                    funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = .Machine$integer.max, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      384                                                    funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = .Machine$double.xmin, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      385                                                    funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = .Machine$double.xmax, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      386                                                                     funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = 1.2, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      387                                                                    funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = -2.5, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      388                                                                    funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = 0+6i, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      389                                                                     funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = NaN, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      390                                                                     funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = Inf, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      391                                                                    funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = -Inf, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      392                                                                    funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = NULL, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      393                                             funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = c(94.3, 1038345.2, 18.2345), ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      394                                        funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = c(1.309605, 0.585381, -0.461072), ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      395                                       funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = c(0.9876, 0.5432, 0.2345, 0.3456), ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      396                                                    funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = c(-1, 0, NaN, 10000), ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      397                                                      funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = c(Inf, -0.5, 1234), ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      398                                                                 funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = c(0, 0), ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      399                                                                funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = c(0, NA), ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      400                                                               funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = numeric(), ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      401                                                funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = c(2L, 5L, 123456L, 100L), ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      402                                                                    funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = -1:3, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      403                                                               funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = c(0L, NA), ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      404                                                               funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = integer(), ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      405                                                                    funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = TRUE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      406                                                      funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = c(TRUE, NA, FALSE), ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      407                                                               funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = logical(), ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      408                                                                 funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = LETTERS, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      409                                                                      funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = "", ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      410                                                                funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = "a test", ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      411                                                          funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = c("", "B", ""), ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      412                                                    funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = c("A", NA, "7", "+"), ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      413                                                             funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = character(), ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      414                                                              funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = factor(""), ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      415                                            funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = factor(c("A", NA, "7", "+")), ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      416                     funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = factor(c("a", NA, "c", "d"), levels = letters[1:4]), ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      417                                               funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = factor(c(NA, NA, NA, NA)), ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      418                              funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = factor(1:4, levels = 10:1, ordered = TRUE), ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      419     funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = factor(c("d", NA, "a", "b"), levels = letters[1:4], ordered = TRUE), ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      420                                                                funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = factor(), ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      421                                                   funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = datasets::iris[, 3:5], ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      422                                                      funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = data.frame(a = NA), ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      423                                                 funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = data.frame(a = letters), ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      424                                            funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = data.frame(a = 1:10, b = NA), ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      425                                           funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = data.frame(a = -1:1, b = Inf), ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      426       funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = data.frame(a = c(12, -2.1, 7), b = c(1.34, 2.28, 0), c = (-1:1 + , ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      427                                                     funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = datasets::iris[0, ], ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      428                                                     funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = datasets::iris[, 0], ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      429                                       funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = datasets::iris[1, , drop = FALSE], ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      430                                       funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = datasets::iris[, 1, drop = FALSE], ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      431                                                            funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = data.frame(), ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      432                                                         funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = matrix(0, 0, 0), ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      433                                                         funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = matrix(1, 1, 0), ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      434                                                         funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = matrix(1, 0, 1), ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      435          funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = matrix(c(1.995874, 1.225707, -0.565287, -2.120309, 0.236326), , ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      436           funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = matrix(c(-1.00577, 0.589365, 0.955131, -0.972982, 1.058721), , ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      437                                         funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = matrix(c(1:7, NA, -1:-4), 3, 4), ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      438                                              funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = matrix(c(-1:6, Inf), 3, 3), ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      439                                                  funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = matrix(letters, 13, 2), ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      440                                                                funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = matrix(), ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      441                                                    funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = array(0, c(0, 0, 0)), ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      442                                                    funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = array(1, c(1, 1, 0)), ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      443                                                    funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = array(1, c(0, 1, 0)), ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      444           funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = array(c(0.7235, -0.19754, 1.67188, -0.62163, -0.84149), c(1, , ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      445          funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = array(c(-0.16868, 0.86973, -0.46976, 0.32157, -0.43558), c(1, , ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      446          funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = array(c(-0.57441, -1.9585, -0.24757, -0.2467, -2.04617), c(5, , ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      447                                           funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = array(c(1:7, NA), c(2, 4, 1)), ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      448                                       funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = array(c(1:5, NA, NA), c(2, 4, 2)), ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      449                                    funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = array(c(1:3, Inf, 12.5), c(2, 4, 2)), ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      450                                                                 funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = array(), ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      451                                                   funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = as.Date("2045-12-25"), ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      452                                                             funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = as.Date(NA), ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      453                                                           funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = as.Date(NULL), ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      454                                                              funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = as.Date(0), ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      455                               funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = as.Date(c("2025-01-01", NA, "930-12-31")), ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      456                                                              funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = Sys.Date(), ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      457            funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = as.POSIXct(c(1048584960, 47764160, 1256110400, 10562745600)), ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      458                                                          funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = as.POSIXct(NA), ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      459                                                        funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = as.POSIXct(NULL), ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      460                                       funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = as.POSIXct("2065-01-01 00:00:00"), ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      461                                    funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = as.POSIXct(0, origin = "1970-01-01"), ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      462                                                  funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = as.POSIXlt(Sys.time()), ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      463                                                              funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = Sys.time(), ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      464                                                          funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = charToRaw("0"), ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      465                                                funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = charToRaw(NA_character_), ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      466                                                        funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = charToRaw("abc"), ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      467                                                                   funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = raw(), ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      468                                                                funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = NA_real_, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      469                                                             funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = NA_integer_, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      470                                                           funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = NA_character_, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      471                                                                      funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = NA, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      472                                                                 funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = list(3), ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      473                                       funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = list(a = c(1, 2, 3), b = letters), ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      474                                                                  funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = list(), ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      475                                                                   funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = 3L, ignore_warnings = FALSE, daemons = 2L, timeout = 2
      476                                                                   funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = 0L, ignore_warnings = FALSE, daemons = 2L, timeout = 2
      477                                                                  funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = -4L, ignore_warnings = FALSE, daemons = 2L, timeout = 2
      478                                                 funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = .Machine$integer.max, ignore_warnings = FALSE, daemons = 2L, timeout = 2
      479                                                 funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = .Machine$double.xmin, ignore_warnings = FALSE, daemons = 2L, timeout = 2
      480                                                 funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = .Machine$double.xmax, ignore_warnings = FALSE, daemons = 2L, timeout = 2
      481                                                                  funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = 1.2, ignore_warnings = FALSE, daemons = 2L, timeout = 2
      482                                                                 funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = -2.5, ignore_warnings = FALSE, daemons = 2L, timeout = 2
      483                                                                 funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = 0+6i, ignore_warnings = FALSE, daemons = 2L, timeout = 2
      484                                                                  funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = NaN, ignore_warnings = FALSE, daemons = 2L, timeout = 2
      485                                                                  funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = Inf, ignore_warnings = FALSE, daemons = 2L, timeout = 2
      486                                                                 funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = -Inf, ignore_warnings = FALSE, daemons = 2L, timeout = 2
      487                                                                 funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = NULL, ignore_warnings = FALSE, daemons = 2L, timeout = 2
      488                                          funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = c(94.3, 1038345.2, 18.2345), ignore_warnings = FALSE, daemons = 2L, timeout = 2
      489                                     funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = c(1.309605, 0.585381, -0.461072), ignore_warnings = FALSE, daemons = 2L, timeout = 2
      490                                    funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = c(0.9876, 0.5432, 0.2345, 0.3456), ignore_warnings = FALSE, daemons = 2L, timeout = 2
      491                                                 funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = c(-1, 0, NaN, 10000), ignore_warnings = FALSE, daemons = 2L, timeout = 2
      492                                                   funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = c(Inf, -0.5, 1234), ignore_warnings = FALSE, daemons = 2L, timeout = 2
      493                                                              funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = c(0, 0), ignore_warnings = FALSE, daemons = 2L, timeout = 2
      494                                                             funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = c(0, NA), ignore_warnings = FALSE, daemons = 2L, timeout = 2
      495                                                            funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = numeric(), ignore_warnings = FALSE, daemons = 2L, timeout = 2
      496                                             funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = c(2L, 5L, 123456L, 100L), ignore_warnings = FALSE, daemons = 2L, timeout = 2
      497                                                                 funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = -1:3, ignore_warnings = FALSE, daemons = 2L, timeout = 2
      498                                                            funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = c(0L, NA), ignore_warnings = FALSE, daemons = 2L, timeout = 2
      499                                                            funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = integer(), ignore_warnings = FALSE, daemons = 2L, timeout = 2
      500                                                                 funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = TRUE, ignore_warnings = FALSE, daemons = 2L, timeout = 2
      501                                                   funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = c(TRUE, NA, FALSE), ignore_warnings = FALSE, daemons = 2L, timeout = 2
      502                                                            funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = logical(), ignore_warnings = FALSE, daemons = 2L, timeout = 2
      503                                                              funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = LETTERS, ignore_warnings = FALSE, daemons = 2L, timeout = 2
      504                                                             funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "a test", ignore_warnings = FALSE, daemons = 2L, timeout = 2
      505                                                       funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = c("", "B", ""), ignore_warnings = FALSE, daemons = 2L, timeout = 2
      506                                                 funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = c("A", NA, "7", "+"), ignore_warnings = FALSE, daemons = 2L, timeout = 2
      507                                                          funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = character(), ignore_warnings = FALSE, daemons = 2L, timeout = 2
      508                                                           funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = factor(""), ignore_warnings = FALSE, daemons = 2L, timeout = 2
      509                                         funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = factor(c("A", NA, "7", "+")), ignore_warnings = FALSE, daemons = 2L, timeout = 2
      510                  funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = factor(c("a", NA, "c", "d"), levels = letters[1:4]), ignore_warnings = FALSE, daemons = 2L, timeout = 2
      511                                            funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = factor(c(NA, NA, NA, NA)), ignore_warnings = FALSE, daemons = 2L, timeout = 2
      512                           funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = factor(1:4, levels = 10:1, ordered = TRUE), ignore_warnings = FALSE, daemons = 2L, timeout = 2
      513  funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = factor(c("d", NA, "a", "b"), levels = letters[1:4], ordered = TRUE), ignore_warnings = FALSE, daemons = 2L, timeout = 2
      514                                                             funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = factor(), ignore_warnings = FALSE, daemons = 2L, timeout = 2
      515                                                funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = datasets::iris[, 3:5], ignore_warnings = FALSE, daemons = 2L, timeout = 2
      516                                                   funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = data.frame(a = NA), ignore_warnings = FALSE, daemons = 2L, timeout = 2
      517                                              funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = data.frame(a = letters), ignore_warnings = FALSE, daemons = 2L, timeout = 2
      518                                         funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = data.frame(a = 1:10, b = NA), ignore_warnings = FALSE, daemons = 2L, timeout = 2
      519                                        funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = data.frame(a = -1:1, b = Inf), ignore_warnings = FALSE, daemons = 2L, timeout = 2
      520    funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = data.frame(a = c(12, -2.1, 7), b = c(1.34, 2.28, 0), c = (-1:1 + , ignore_warnings = FALSE, daemons = 2L, timeout = 2
      521                                                  funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = datasets::iris[0, ], ignore_warnings = FALSE, daemons = 2L, timeout = 2
      522                                                  funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = datasets::iris[, 0], ignore_warnings = FALSE, daemons = 2L, timeout = 2
      523                                    funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = datasets::iris[1, , drop = FALSE], ignore_warnings = FALSE, daemons = 2L, timeout = 2
      524                                    funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = datasets::iris[, 1, drop = FALSE], ignore_warnings = FALSE, daemons = 2L, timeout = 2
      525                                                         funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = data.frame(), ignore_warnings = FALSE, daemons = 2L, timeout = 2
      526                                                      funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = matrix(0, 0, 0), ignore_warnings = FALSE, daemons = 2L, timeout = 2
      527                                                      funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = matrix(1, 1, 0), ignore_warnings = FALSE, daemons = 2L, timeout = 2
      528                                                      funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = matrix(1, 0, 1), ignore_warnings = FALSE, daemons = 2L, timeout = 2
      529       funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = matrix(c(1.995874, 1.225707, -0.565287, -2.120309, 0.236326), , ignore_warnings = FALSE, daemons = 2L, timeout = 2
      530        funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = matrix(c(-1.00577, 0.589365, 0.955131, -0.972982, 1.058721), , ignore_warnings = FALSE, daemons = 2L, timeout = 2
      531                                      funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = matrix(c(1:7, NA, -1:-4), 3, 4), ignore_warnings = FALSE, daemons = 2L, timeout = 2
      532                                           funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = matrix(c(-1:6, Inf), 3, 3), ignore_warnings = FALSE, daemons = 2L, timeout = 2
      533                                               funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = matrix(letters, 13, 2), ignore_warnings = FALSE, daemons = 2L, timeout = 2
      534                                                             funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = matrix(), ignore_warnings = FALSE, daemons = 2L, timeout = 2
      535                                                 funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = array(0, c(0, 0, 0)), ignore_warnings = FALSE, daemons = 2L, timeout = 2
      536                                                 funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = array(1, c(1, 1, 0)), ignore_warnings = FALSE, daemons = 2L, timeout = 2
      537                                                 funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = array(1, c(0, 1, 0)), ignore_warnings = FALSE, daemons = 2L, timeout = 2
      538        funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = array(c(0.7235, -0.19754, 1.67188, -0.62163, -0.84149), c(1, , ignore_warnings = FALSE, daemons = 2L, timeout = 2
      539       funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = array(c(-0.16868, 0.86973, -0.46976, 0.32157, -0.43558), c(1, , ignore_warnings = FALSE, daemons = 2L, timeout = 2
      540       funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = array(c(-0.57441, -1.9585, -0.24757, -0.2467, -2.04617), c(5, , ignore_warnings = FALSE, daemons = 2L, timeout = 2
      541                                        funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = array(c(1:7, NA), c(2, 4, 1)), ignore_warnings = FALSE, daemons = 2L, timeout = 2
      542                                    funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = array(c(1:5, NA, NA), c(2, 4, 2)), ignore_warnings = FALSE, daemons = 2L, timeout = 2
      543                                 funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = array(c(1:3, Inf, 12.5), c(2, 4, 2)), ignore_warnings = FALSE, daemons = 2L, timeout = 2
      544                                                              funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = array(), ignore_warnings = FALSE, daemons = 2L, timeout = 2
      545                                                funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = as.Date("2045-12-25"), ignore_warnings = FALSE, daemons = 2L, timeout = 2
      546                                                          funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = as.Date(NA), ignore_warnings = FALSE, daemons = 2L, timeout = 2
      547                                                        funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = as.Date(NULL), ignore_warnings = FALSE, daemons = 2L, timeout = 2
      548                                                           funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = as.Date(0), ignore_warnings = FALSE, daemons = 2L, timeout = 2
      549                            funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = as.Date(c("2025-01-01", NA, "930-12-31")), ignore_warnings = FALSE, daemons = 2L, timeout = 2
      550                                                           funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = Sys.Date(), ignore_warnings = FALSE, daemons = 2L, timeout = 2
      551         funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = as.POSIXct(c(1048584960, 47764160, 1256110400, 10562745600)), ignore_warnings = FALSE, daemons = 2L, timeout = 2
      552                                                       funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = as.POSIXct(NA), ignore_warnings = FALSE, daemons = 2L, timeout = 2
      553                                                     funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = as.POSIXct(NULL), ignore_warnings = FALSE, daemons = 2L, timeout = 2
      554                                    funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = as.POSIXct("2065-01-01 00:00:00"), ignore_warnings = FALSE, daemons = 2L, timeout = 2
      555                                 funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = as.POSIXct(0, origin = "1970-01-01"), ignore_warnings = FALSE, daemons = 2L, timeout = 2
      556                                               funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = as.POSIXlt(Sys.time()), ignore_warnings = FALSE, daemons = 2L, timeout = 2
      557                                                           funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = Sys.time(), ignore_warnings = FALSE, daemons = 2L, timeout = 2
      558                                                       funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = charToRaw("0"), ignore_warnings = FALSE, daemons = 2L, timeout = 2
      559                                             funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = charToRaw(NA_character_), ignore_warnings = FALSE, daemons = 2L, timeout = 2
      560                                                     funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = charToRaw("abc"), ignore_warnings = FALSE, daemons = 2L, timeout = 2
      561                                                                funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = raw(), ignore_warnings = FALSE, daemons = 2L, timeout = 2
      562                                                             funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = NA_real_, ignore_warnings = FALSE, daemons = 2L, timeout = 2
      563                                                          funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = NA_integer_, ignore_warnings = FALSE, daemons = 2L, timeout = 2
      564                                                        funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = NA_character_, ignore_warnings = FALSE, daemons = 2L, timeout = 2
      565                                                                   funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = NA, ignore_warnings = FALSE, daemons = 2L, timeout = 2
      566                                                              funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = list(3), ignore_warnings = FALSE, daemons = 2L, timeout = 2
      567                                    funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = list(a = c(1, 2, 3), b = letters), ignore_warnings = FALSE, daemons = 2L, timeout = 2
      568                                                               funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = list(), ignore_warnings = FALSE, daemons = 2L, timeout = 2
      569                                                                      funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = 3L, daemons = 2L, timeout = 2
      570                                                                      funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = 0L, daemons = 2L, timeout = 2
      571                                                                     funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = -4L, daemons = 2L, timeout = 2
      572                                                    funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = .Machine$integer.max, daemons = 2L, timeout = 2
      573                                                    funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = .Machine$double.xmin, daemons = 2L, timeout = 2
      574                                                    funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = .Machine$double.xmax, daemons = 2L, timeout = 2
      575                                                                     funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = 1.2, daemons = 2L, timeout = 2
      576                                                                    funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = -2.5, daemons = 2L, timeout = 2
      577                                                                    funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = 0+6i, daemons = 2L, timeout = 2
      578                                                                     funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = NaN, daemons = 2L, timeout = 2
      579                                                                     funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = Inf, daemons = 2L, timeout = 2
      580                                                                    funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = -Inf, daemons = 2L, timeout = 2
      581                                                                    funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = NULL, daemons = 2L, timeout = 2
      582                                             funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = c(94.3, 1038345.2, 18.2345), daemons = 2L, timeout = 2
      583                                        funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = c(1.309605, 0.585381, -0.461072), daemons = 2L, timeout = 2
      584                                       funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = c(0.9876, 0.5432, 0.2345, 0.3456), daemons = 2L, timeout = 2
      585                                                    funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = c(-1, 0, NaN, 10000), daemons = 2L, timeout = 2
      586                                                      funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = c(Inf, -0.5, 1234), daemons = 2L, timeout = 2
      587                                                                 funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = c(0, 0), daemons = 2L, timeout = 2
      588                                                                funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = c(0, NA), daemons = 2L, timeout = 2
      589                                                               funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = numeric(), daemons = 2L, timeout = 2
      590                                                funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = c(2L, 5L, 123456L, 100L), daemons = 2L, timeout = 2
      591                                                                    funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = -1:3, daemons = 2L, timeout = 2
      592                                                               funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = c(0L, NA), daemons = 2L, timeout = 2
      593                                                               funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = integer(), daemons = 2L, timeout = 2
      594                                                                    funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = TRUE, daemons = 2L, timeout = 2
      595                                                      funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = c(TRUE, NA, FALSE), daemons = 2L, timeout = 2
      596                                                               funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = logical(), daemons = 2L, timeout = 2
      597                                                                 funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = LETTERS, daemons = 2L, timeout = 2
      598                                                                      funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = "", daemons = 2L, timeout = 2
      599                                                                funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = "a test", daemons = 2L, timeout = 2
      600                                                          funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = c("", "B", ""), daemons = 2L, timeout = 2
      601                                                    funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = c("A", NA, "7", "+"), daemons = 2L, timeout = 2
      602                                                             funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = character(), daemons = 2L, timeout = 2
      603                                                              funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = factor(""), daemons = 2L, timeout = 2
      604                                            funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = factor(c("A", NA, "7", "+")), daemons = 2L, timeout = 2
      605                     funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = factor(c("a", NA, "c", "d"), levels = letters[1:4]), daemons = 2L, timeout = 2
      606                                               funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = factor(c(NA, NA, NA, NA)), daemons = 2L, timeout = 2
      607                              funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = factor(1:4, levels = 10:1, ordered = TRUE), daemons = 2L, timeout = 2
      608     funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = factor(c("d", NA, "a", "b"), levels = letters[1:4], ordered = TRUE), daemons = 2L, timeout = 2
      609                                                                funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = factor(), daemons = 2L, timeout = 2
      610                                                   funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = datasets::iris[, 3:5], daemons = 2L, timeout = 2
      611                                                      funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = data.frame(a = NA), daemons = 2L, timeout = 2
      612                                                 funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = data.frame(a = letters), daemons = 2L, timeout = 2
      613                                            funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = data.frame(a = 1:10, b = NA), daemons = 2L, timeout = 2
      614                                           funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = data.frame(a = -1:1, b = Inf), daemons = 2L, timeout = 2
      615       funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = data.frame(a = c(12, -2.1, 7), b = c(1.34, 2.28, 0), c = (-1:1 + , daemons = 2L, timeout = 2
      616                                                     funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = datasets::iris[0, ], daemons = 2L, timeout = 2
      617                                                     funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = datasets::iris[, 0], daemons = 2L, timeout = 2
      618                                       funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = datasets::iris[1, , drop = FALSE], daemons = 2L, timeout = 2
      619                                       funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = datasets::iris[, 1, drop = FALSE], daemons = 2L, timeout = 2
      620                                                            funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = data.frame(), daemons = 2L, timeout = 2
      621                                                         funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = matrix(0, 0, 0), daemons = 2L, timeout = 2
      622                                                         funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = matrix(1, 1, 0), daemons = 2L, timeout = 2
      623                                                         funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = matrix(1, 0, 1), daemons = 2L, timeout = 2
      624          funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = matrix(c(1.995874, 1.225707, -0.565287, -2.120309, 0.236326), , daemons = 2L, timeout = 2
      625           funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = matrix(c(-1.00577, 0.589365, 0.955131, -0.972982, 1.058721), , daemons = 2L, timeout = 2
      626                                         funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = matrix(c(1:7, NA, -1:-4), 3, 4), daemons = 2L, timeout = 2
      627                                              funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = matrix(c(-1:6, Inf), 3, 3), daemons = 2L, timeout = 2
      628                                                  funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = matrix(letters, 13, 2), daemons = 2L, timeout = 2
      629                                                                funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = matrix(), daemons = 2L, timeout = 2
      630                                                    funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = array(0, c(0, 0, 0)), daemons = 2L, timeout = 2
      631                                                    funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = array(1, c(1, 1, 0)), daemons = 2L, timeout = 2
      632                                                    funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = array(1, c(0, 1, 0)), daemons = 2L, timeout = 2
      633           funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = array(c(0.7235, -0.19754, 1.67188, -0.62163, -0.84149), c(1, , daemons = 2L, timeout = 2
      634          funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = array(c(-0.16868, 0.86973, -0.46976, 0.32157, -0.43558), c(1, , daemons = 2L, timeout = 2
      635          funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = array(c(-0.57441, -1.9585, -0.24757, -0.2467, -2.04617), c(5, , daemons = 2L, timeout = 2
      636                                           funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = array(c(1:7, NA), c(2, 4, 1)), daemons = 2L, timeout = 2
      637                                       funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = array(c(1:5, NA, NA), c(2, 4, 2)), daemons = 2L, timeout = 2
      638                                    funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = array(c(1:3, Inf, 12.5), c(2, 4, 2)), daemons = 2L, timeout = 2
      639                                                                 funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = array(), daemons = 2L, timeout = 2
      640                                                   funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = as.Date("2045-12-25"), daemons = 2L, timeout = 2
      641                                                             funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = as.Date(NA), daemons = 2L, timeout = 2
      642                                                           funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = as.Date(NULL), daemons = 2L, timeout = 2
      643                                                              funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = as.Date(0), daemons = 2L, timeout = 2
      644                               funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = as.Date(c("2025-01-01", NA, "930-12-31")), daemons = 2L, timeout = 2
      645                                                              funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = Sys.Date(), daemons = 2L, timeout = 2
      646            funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = as.POSIXct(c(1048584960, 47764160, 1256110400, 10562745600)), daemons = 2L, timeout = 2
      647                                                          funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = as.POSIXct(NA), daemons = 2L, timeout = 2
      648                                                        funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = as.POSIXct(NULL), daemons = 2L, timeout = 2
      649                                       funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = as.POSIXct("2065-01-01 00:00:00"), daemons = 2L, timeout = 2
      650                                    funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = as.POSIXct(0, origin = "1970-01-01"), daemons = 2L, timeout = 2
      651                                                  funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = as.POSIXlt(Sys.time()), daemons = 2L, timeout = 2
      652                                                              funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = Sys.time(), daemons = 2L, timeout = 2
      653                                                          funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = charToRaw("0"), daemons = 2L, timeout = 2
      654                                                funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = charToRaw(NA_character_), daemons = 2L, timeout = 2
      655                                                        funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = charToRaw("abc"), daemons = 2L, timeout = 2
      656                                                                   funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = raw(), daemons = 2L, timeout = 2
      657                                                                funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = NA_real_, daemons = 2L, timeout = 2
      658                                                             funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = NA_integer_, daemons = 2L, timeout = 2
      659                                                           funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = NA_character_, daemons = 2L, timeout = 2
      660                                                                      funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = NA, daemons = 2L, timeout = 2
      661                                                                 funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = list(3), daemons = 2L, timeout = 2
      662                                       funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = list(a = c(1, 2, 3), b = letters), daemons = 2L, timeout = 2
      663                                                                  funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = list(), daemons = 2L, timeout = 2
      664                                                                   funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 3L, timeout = 2
      665                                                                   funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 0L, timeout = 2
      666                                                                  funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = -4L, timeout = 2
      667                                                 funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = .Machine$integer.max, timeout = 2
      668                                                 funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = .Machine$double.xmin, timeout = 2
      669                                                 funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = .Machine$double.xmax, timeout = 2
      670                                                                  funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 1.2, timeout = 2
      671                                                                 funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = -2.5, timeout = 2
      672                                                                 funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 0+6i, timeout = 2
      673                                                                  funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = NaN, timeout = 2
      674                                                                  funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = Inf, timeout = 2
      675                                                                 funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = -Inf, timeout = 2
      676                                                                 funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = NULL, timeout = 2
      677                                          funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = c(94.3, 1038345.2, 18.2345), timeout = 2
      678                                     funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = c(1.309605, 0.585381, -0.461072), timeout = 2
      679                                    funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = c(0.9876, 0.5432, 0.2345, 0.3456), timeout = 2
      680                                                 funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = c(-1, 0, NaN, 10000), timeout = 2
      681                                                   funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = c(Inf, -0.5, 1234), timeout = 2
      682                                                              funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = c(0, 0), timeout = 2
      683                                                             funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = c(0, NA), timeout = 2
      684                                                            funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = numeric(), timeout = 2
      685                                             funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = c(2L, 5L, 123456L, 100L), timeout = 2
      686                                                                 funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = -1:3, timeout = 2
      687                                                            funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = c(0L, NA), timeout = 2
      688                                                            funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = integer(), timeout = 2
      689                                                                 funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = TRUE, timeout = 2
      690                                                   funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = c(TRUE, NA, FALSE), timeout = 2
      691                                                            funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = logical(), timeout = 2
      692                                                              funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = LETTERS, timeout = 2
      693                                                                   funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = "", timeout = 2
      694                                                             funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = "a test", timeout = 2
      695                                                       funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = c("", "B", ""), timeout = 2
      696                                                 funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = c("A", NA, "7", "+"), timeout = 2
      697                                                          funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = character(), timeout = 2
      698                                                           funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = factor(""), timeout = 2
      699                                         funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = factor(c("A", NA, "7", "+")), timeout = 2
      700                  funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = factor(c("a", NA, "c", "d"), levels = letters[1:4]), timeout = 2
      701                                            funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = factor(c(NA, NA, NA, NA)), timeout = 2
      702                           funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = factor(1:4, levels = 10:1, ordered = TRUE), timeout = 2
      703  funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = factor(c("d", NA, "a", "b"), levels = letters[1:4], ordered = TRUE), timeout = 2
      704                                                             funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = factor(), timeout = 2
      705                                                funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = datasets::iris[, 3:5], timeout = 2
      706                                                   funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = data.frame(a = NA), timeout = 2
      707                                              funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = data.frame(a = letters), timeout = 2
      708                                         funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = data.frame(a = 1:10, b = NA), timeout = 2
      709                                        funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = data.frame(a = -1:1, b = Inf), timeout = 2
      710    funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = data.frame(a = c(12, -2.1, 7), b = c(1.34, 2.28, 0), c = (-1:1 + , timeout = 2
      711                                                  funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = datasets::iris[0, ], timeout = 2
      712                                                  funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = datasets::iris[, 0], timeout = 2
      713                                    funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = datasets::iris[1, , drop = FALSE], timeout = 2
      714                                    funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = datasets::iris[, 1, drop = FALSE], timeout = 2
      715                                                         funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = data.frame(), timeout = 2
      716                                                      funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = matrix(0, 0, 0), timeout = 2
      717                                                      funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = matrix(1, 1, 0), timeout = 2
      718                                                      funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = matrix(1, 0, 1), timeout = 2
      719       funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = matrix(c(1.995874, 1.225707, -0.565287, -2.120309, 0.236326), , timeout = 2
      720        funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = matrix(c(-1.00577, 0.589365, 0.955131, -0.972982, 1.058721), , timeout = 2
      721                                      funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = matrix(c(1:7, NA, -1:-4), 3, 4), timeout = 2
      722                                           funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = matrix(c(-1:6, Inf), 3, 3), timeout = 2
      723                                               funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = matrix(letters, 13, 2), timeout = 2
      724                                                             funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = matrix(), timeout = 2
      725                                                 funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = array(0, c(0, 0, 0)), timeout = 2
      726                                                 funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = array(1, c(1, 1, 0)), timeout = 2
      727                                                 funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = array(1, c(0, 1, 0)), timeout = 2
      728        funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = array(c(0.7235, -0.19754, 1.67188, -0.62163, -0.84149), c(1, , timeout = 2
      729       funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = array(c(-0.16868, 0.86973, -0.46976, 0.32157, -0.43558), c(1, , timeout = 2
      730       funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = array(c(-0.57441, -1.9585, -0.24757, -0.2467, -2.04617), c(5, , timeout = 2
      731                                        funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = array(c(1:7, NA), c(2, 4, 1)), timeout = 2
      732                                    funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = array(c(1:5, NA, NA), c(2, 4, 2)), timeout = 2
      733                                 funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = array(c(1:3, Inf, 12.5), c(2, 4, 2)), timeout = 2
      734                                                              funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = array(), timeout = 2
      735                                                funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = as.Date("2045-12-25"), timeout = 2
      736                                                          funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = as.Date(NA), timeout = 2
      737                                                        funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = as.Date(NULL), timeout = 2
      738                                                           funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = as.Date(0), timeout = 2
      739                            funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = as.Date(c("2025-01-01", NA, "930-12-31")), timeout = 2
      740                                                           funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = Sys.Date(), timeout = 2
      741         funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = as.POSIXct(c(1048584960, 47764160, 1256110400, 10562745600)), timeout = 2
      742                                                       funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = as.POSIXct(NA), timeout = 2
      743                                                     funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = as.POSIXct(NULL), timeout = 2
      744                                    funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = as.POSIXct("2065-01-01 00:00:00"), timeout = 2
      745                                 funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = as.POSIXct(0, origin = "1970-01-01"), timeout = 2
      746                                               funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = as.POSIXlt(Sys.time()), timeout = 2
      747                                                           funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = Sys.time(), timeout = 2
      748                                                       funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = charToRaw("0"), timeout = 2
      749                                             funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = charToRaw(NA_character_), timeout = 2
      750                                                     funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = charToRaw("abc"), timeout = 2
      751                                                                funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = raw(), timeout = 2
      752                                                             funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = NA_real_, timeout = 2
      753                                                          funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = NA_integer_, timeout = 2
      754                                                        funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = NA_character_, timeout = 2
      755                                                                   funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = NA, timeout = 2
      756                                                              funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = list(3), timeout = 2
      757                                    funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = list(a = c(1, 2, 3), b = letters), timeout = 2
      758                                                               funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = list(), timeout = 2
      759                                                                  funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 3L
      760                                                                  funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 0L
      761                                                                 funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = -4L
      762                                                funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = .Machine$integer.max
      763                                                funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = .Machine$double.xmin
      764                                                funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = .Machine$double.xmax
      765                                                                 funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 1.2
      766                                                                funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = -2.5
      767                                                                funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = 0+6i
      768                                                                 funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = NaN
      769                                                                 funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = Inf
      770                                                                funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = -Inf
      771                                                                funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = NULL
      772                                         funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = c(94.3, 1038345.2, 18.2345)
      773                                    funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = c(1.309605, 0.585381, -0.461072)
      774                                   funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = c(0.9876, 0.5432, 0.2345, 0.3456)
      775                                                funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = c(-1, 0, NaN, 10000)
      776                                                  funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = c(Inf, -0.5, 1234)
      777                                                             funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = c(0, 0)
      778                                                            funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = c(0, NA)
      779                                                           funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = numeric()
      780                                            funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = c(2L, 5L, 123456L, 100L)
      781                                                                funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = -1:3
      782                                                           funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = c(0L, NA)
      783                                                           funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = integer()
      784                                                                funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = TRUE
      785                                                  funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = c(TRUE, NA, FALSE)
      786                                                           funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = logical()
      787                                                             funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = LETTERS
      788                                                                  funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = ""
      789                                                            funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = "a test"
      790                                                      funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = c("", "B", "")
      791                                                funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = c("A", NA, "7", "+")
      792                                                         funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = character()
      793                                                          funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = factor("")
      794                                        funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = factor(c("A", NA, "7", "+"))
      795                 funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = factor(c("a", NA, "c", "d"), levels = letters[1:4])
      796                                           funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = factor(c(NA, NA, NA, NA))
      797                          funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = factor(1:4, levels = 10:1, ordered = TRUE)
      798 funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = factor(c("d", NA, "a", "b"), levels = letters[1:4], ordered = TRUE)
      799                                                            funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = factor()
      800                                               funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = datasets::iris[, 3:5]
      801                                                  funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = data.frame(a = NA)
      802                                             funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = data.frame(a = letters)
      803                                        funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = data.frame(a = 1:10, b = NA)
      804                                       funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = data.frame(a = -1:1, b = Inf)
      805   funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = data.frame(a = c(12, -2.1, 7), b = c(1.34, 2.28, 0), c = (-1:1 + 
      806                                                 funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = datasets::iris[0, ]
      807                                                 funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = datasets::iris[, 0]
      808                                   funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = datasets::iris[1, , drop = FALSE]
      809                                   funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = datasets::iris[, 1, drop = FALSE]
      810                                                        funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = data.frame()
      811                                                     funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = matrix(0, 0, 0)
      812                                                     funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = matrix(1, 1, 0)
      813                                                     funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = matrix(1, 0, 1)
      814      funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = matrix(c(1.995874, 1.225707, -0.565287, -2.120309, 0.236326), 
      815       funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = matrix(c(-1.00577, 0.589365, 0.955131, -0.972982, 1.058721), 
      816                                     funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = matrix(c(1:7, NA, -1:-4), 3, 4)
      817                                          funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = matrix(c(-1:6, Inf), 3, 3)
      818                                              funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = matrix(letters, 13, 2)
      819                                                            funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = matrix()
      820                                                funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = array(0, c(0, 0, 0))
      821                                                funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = array(1, c(1, 1, 0))
      822                                                funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = array(1, c(0, 1, 0))
      823       funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = array(c(0.7235, -0.19754, 1.67188, -0.62163, -0.84149), c(1, 
      824      funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = array(c(-0.16868, 0.86973, -0.46976, 0.32157, -0.43558), c(1, 
      825      funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = array(c(-0.57441, -1.9585, -0.24757, -0.2467, -2.04617), c(5, 
      826                                       funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = array(c(1:7, NA), c(2, 4, 1))
      827                                   funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = array(c(1:5, NA, NA), c(2, 4, 2))
      828                                funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = array(c(1:3, Inf, 12.5), c(2, 4, 2))
      829                                                             funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = array()
      830                                               funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = as.Date("2045-12-25")
      831                                                         funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = as.Date(NA)
      832                                                       funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = as.Date(NULL)
      833                                                          funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = as.Date(0)
      834                           funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = as.Date(c("2025-01-01", NA, "930-12-31"))
      835                                                          funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = Sys.Date()
      836        funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = as.POSIXct(c(1048584960, 47764160, 1256110400, 10562745600))
      837                                                      funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = as.POSIXct(NA)
      838                                                    funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = as.POSIXct(NULL)
      839                                   funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = as.POSIXct("2065-01-01 00:00:00")
      840                                funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = as.POSIXct(0, origin = "1970-01-01")
      841                                              funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = as.POSIXlt(Sys.time())
      842                                                          funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = Sys.time()
      843                                                      funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = charToRaw("0")
      844                                            funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = charToRaw(NA_character_)
      845                                                    funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = charToRaw("abc")
      846                                                               funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = raw()
      847                                                            funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = NA_real_
      848                                                         funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = NA_integer_
      849                                                       funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = NA_character_
      850                                                                  funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = NA
      851                                                             funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = list(3)
      852                                   funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = list(a = c(1, 2, 3), b = letters)
      853                                                              funs = "list", what = list(NULL), args = NULL, package = NULL, listify_what = FALSE, ignore_patterns = "", ignore_warnings = FALSE, daemons = 2L, timeout = list()

# whitelist

    Code
      whitelist(res, NA)
    Condition
      Error in `whitelist()`:
      ! `patterns` should be of class <character>

---

    Code
      whitelist(res, character(0))
    Condition
      Error in `whitelist()`:
      ! `patterns` is an empty <character>

# get_exported_functions

    Code
      get_exported_functions("nonexistent")
    Condition
      Error in `get_exported_functions()`:
      ! there is no package called 'nonexistent'

---

    Code
      get_exported_functions("CBTF", character(0))
    Condition
      Error in `get_exported_functions()`:
      ! `ignore_names` is an empty <character>

