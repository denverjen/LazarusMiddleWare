unit dgfunc;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLType, LCLProc, CsvDocument, zstream, Variants,lazutf8 ;

function dgSTOD( mStr : String ) : TDateTime ;
function dgDTOS( mDate : TDateTime ) : String ;
function dgExtractString( mStr : String ) : String ;
function dgTranslateMode : Integer ;
function dgTrad2Simp( mStr : String ) : String ;
function dgSimp2Trad( mStr : String ) : String ;
function dgGetParaString(mText: String; mAt: Integer): String;
procedure dgCompressStream( mFromStream,mToStream : TMemoryStream ) ;
procedure dgDeCompressStream( mFromStream,mToStream : TMemoryStream ) ;
function dgStreamReadLine( mStream : TMemoryStream ) : String  ;
procedure dgCopyStream( mFrom, mTo : TMemoryStream ) ;
function dgStreamToDateTime( mMem : TMemoryStream ) : TDateTime ;
procedure dgStreamAddLines( mStr : String ; mStream : TMemoryStream ) ;
procedure dgArrayOfVariantToStream( Const mVar : Array of Variant ; mStream : TMemoryStream ) ;
procedure dgVariantToStream( mVar : Variant ; mStream : TMemoryStream ) ;
function StreamToVariant(stream:TStream):variant;
procedure VariantToStream(AVariant:variant; stream:TStream);
function dgStrToVariant( mStr : String ) : Variant ;
function dgStreamToStr( mMem : TMemoryStream ) : String ;
procedure dgDateTimeToStream( mDateTime : TDateTime ; mMem : TMemoryStream ) ;
procedure dgStrToStream( mStr : String ; mMem : TMemoryStream ) ;
procedure dgIntegerToStream( mInt : Integer ; mMem : TMemoryStream ) ;
function dgStreamToInteger( mMem : TMemoryStream ) : Integer ;
procedure dgLongIntToStream( mLong : LongInt ; mMem : TMemoryStream ) ;
procedure dgFloatToStream( mFloat : Double ; mMem : TMemoryStream ) ;
procedure dgStreamCopyToStream( mSource, mDest : TMemoryStream ; mSize : LongInt ) ;
procedure dgStreamLoadFromStream( mSource, mDest : TMemoryStream ) ;
procedure dgCurrencyToStream( mCurr : Currency ; mMem : TMemoryStream ) ;
procedure dgBooleanToStream( mBool : Boolean ; mMem : TMemoryStream ) ;
function dgIntToBool( mInt : Integer ) : Boolean ;
function dgBoolToInt( mBool : Boolean ) : Integer ;
function dgStreamToLongInt( mMem : TMemoryStream ) : LongInt ;
function dgStreamToFloat( mMem : TMemoryStream ) : Double ;
function dgStreamToCurrency( mMem : TMemoryStream ) : Currency ;
function dgStreamToBoolean( mMem : TMemoryStream ) : Boolean ;

implementation

function dgSTOD(mStr: String): TDateTime;
var
  lDay,lMonth,lYear : Word ;
begin
  lDay := 1 ; lMonth := 1 ; lYear := 1980 ;
  if Length( mStr ) = 8 then
    begin
    lYear := StrToIntDef( Copy( mStr,1,4 ), 1980 ) ;
    lMonth := StrToIntDef( Copy( mStr,5,2 ), 1 ) ;
    lDay := StrToIntDef( Copy( mStr,7,2 ), 1 ) ;
    end ;
  Result := EncodeDate( lYear,lMonth,lDay ) ;
end;

function dgDTOS(mDate: TDateTime): String;
begin
    Result := FormatDateTime( 'yyyymmdd',mDate ) ;
end;

function dgExtractString( mStr : String ) : String ;
begin
  Result := dgGetParaString( mStr, 1 ) ;
end;

function dgTranslateMode : Integer ;
begin
  Result := 0 ;  // no translation
 // Result := 1 ;  // force simpified chinese character
  // Result := 2 ;  // force tranditional chinese character
end;

function dgSimp2Trad( mStr : String ) : String ;
var
  mtc,msc,mchar : String ;
  mCount,i,mat : Integer ;
begin
  msc := '皑蔼碍爱翱袄奥坝罢摆败颁办绊帮绑镑谤剥饱宝报鲍辈贝钡狈备惫绷笔毕毙币闭边编贬变辩辫标鳖别瘪濒滨宾摈饼并拨钵铂驳卜补财参蚕残惭惨灿苍舱仓沧厕侧册测层诧搀掺蝉馋谗缠铲产阐颤场尝长偿肠厂畅钞车彻尘沉陈衬撑称惩诚骋痴迟驰耻齿炽冲虫宠畴踌筹绸丑橱厨锄雏础储触处传疮闯创锤纯绰辞词赐聪葱囱从丛凑蹿窜错达带贷担单郸掸胆惮诞弹当挡党荡档捣岛祷导盗灯邓敌涤递缔颠点垫电淀钓调迭谍叠钉顶锭订丢东动栋冻斗犊独读赌镀锻断缎兑队对吨顿钝夺堕鹅额讹恶饿儿尔饵贰发罚阀珐矾钒烦范贩饭访纺飞诽废费纷坟奋愤粪丰枫锋风疯冯缝讽凤肤辐抚辅赋复负讣妇缚该钙盖干赶秆赣冈刚钢纲岗皋镐搁鸽阁铬个给龚宫巩贡钩沟构购够蛊顾剐关观馆惯贯广规硅归龟闺轨诡柜贵刽辊滚锅国过骇韩汉号阂鹤贺横轰鸿红后壶护沪户哗华画划话怀坏欢环还缓换唤痪焕涣黄谎挥辉毁贿秽会烩汇讳诲绘荤浑伙获货祸击机积饥讥鸡绩缉极辑级挤几蓟剂济计记际继纪夹荚颊贾钾价驾歼监坚笺间艰缄茧检碱硷拣捡简俭减荐槛鉴践贱见键舰剑饯渐溅涧将浆蒋桨奖讲酱胶浇骄娇搅铰矫侥脚饺缴绞轿较秸阶节茎鲸惊经颈静镜径痉竞净纠厩旧驹举据锯惧剧鹃绢杰洁结诫届紧锦仅谨进晋烬尽劲荆觉决诀绝钧军骏开凯颗壳课垦恳抠库裤夸块侩宽矿旷况亏岿窥馈溃扩阔蜡腊莱来赖蓝栏拦篮阑兰澜谰揽览懒缆烂滥捞劳涝乐镭垒类泪篱离里鲤礼丽厉励砾历沥隶俩联莲连镰怜涟帘敛脸链恋炼练粮凉两辆谅疗辽镣猎临邻鳞凛赁龄铃凌灵岭领馏刘龙聋咙笼垄拢陇楼娄搂篓芦卢颅庐炉掳卤虏鲁赂禄录陆驴吕铝侣屡缕虑滤绿峦挛孪滦乱抡轮伦仑沦纶论萝罗逻锣箩骡骆络妈玛码蚂马骂吗买麦卖迈脉瞒馒蛮满谩猫锚铆贸么霉没镁门闷们锰梦谜弥觅幂绵缅庙灭悯闽鸣铭谬谋亩钠纳难挠脑恼闹馁内拟腻撵捻酿鸟聂啮镊镍柠狞宁拧泞钮纽脓浓农疟诺欧鸥殴呕沤盘庞赔喷鹏骗飘频贫苹凭评泼颇扑铺朴谱栖凄脐齐骑岂启气弃讫牵扦钎铅迁签谦钱钳潜浅谴堑枪呛墙蔷强抢锹桥乔侨翘窍窃钦亲寝轻氢倾顷请庆琼穷趋区躯驱龋颧权劝却鹊确让饶扰绕热韧认纫荣绒软锐闰润洒萨鳃赛叁伞丧骚扫涩杀纱筛晒删闪陕赡缮伤赏烧绍赊摄慑设绅审婶肾渗声绳胜圣师狮湿诗尸时蚀实识驶势适释饰视试寿兽枢输书赎属术树竖数帅双谁税顺说硕烁丝饲耸怂颂讼诵擞苏诉肃虽随绥岁孙损笋缩琐锁獭挞抬态摊贪瘫滩坛谭谈叹汤烫涛绦讨腾誊锑题体屉条贴铁厅听烃铜统头秃图涂团颓蜕脱鸵驮驼椭洼袜弯湾顽万网韦违围为潍维苇伟伪纬谓卫温闻纹稳问瓮挝蜗涡窝卧呜钨乌污诬无芜吴坞雾务误锡牺袭习铣戏细虾辖峡侠狭厦吓锨鲜纤咸贤衔闲显险现献县馅羡宪线厢镶乡详响项萧嚣销晓啸蝎协挟携胁谐写泻谢锌衅兴汹锈绣虚嘘须许叙绪续轩悬选癣绚学勋询寻驯训讯逊压鸦鸭哑亚讶阉烟盐严颜阎艳厌砚彦谚验鸯杨扬疡阳痒养样瑶摇尧遥窑谣药爷页业叶医铱颐遗仪彝蚁艺亿忆义诣议谊译异绎荫阴银饮隐樱婴鹰应缨莹萤营荧蝇赢颖哟拥佣痈踊咏涌优忧邮铀犹游诱舆鱼渔娱与屿语吁御狱誉预驭鸳渊辕园员圆缘远愿约跃钥岳粤悦阅云郧匀陨运蕴酝晕韵杂灾载攒暂赞赃脏凿枣灶责择则泽贼赠扎札轧铡闸栅诈斋债毡盏斩辗崭栈战绽张涨帐账胀赵蛰辙锗这贞针侦诊镇阵挣睁狰争帧郑证织职执纸挚掷帜质滞钟终种肿众诌轴皱昼骤猪诸诛烛瞩嘱贮铸筑驻专砖转赚桩庄装妆壮状锥赘坠缀谆着浊兹资渍踪综总纵邹诅组钻' ;
  mtc := '皚藹礙愛翺襖奧壩罷擺敗頒辦絆幫綁鎊謗剝飽寶報鮑輩貝鋇狽備憊繃筆畢斃幣閉邊編貶變辯辮標鼈別癟瀕濱賓擯餅並撥缽鉑駁蔔補財參蠶殘慚慘燦蒼艙倉滄廁側冊測層詫攙摻蟬饞讒纏鏟産闡顫場嘗長償腸廠暢鈔車徹塵沈陳襯撐稱懲誠騁癡遲馳恥齒熾沖蟲寵疇躊籌綢醜櫥廚鋤雛礎儲觸處傳瘡闖創錘純綽辭詞賜聰蔥囪從叢湊躥竄錯達帶貸擔單鄲撣膽憚誕彈當擋黨蕩檔搗島禱導盜燈鄧敵滌遞締顛點墊電澱釣調叠諜疊釘頂錠訂丟東動棟凍鬥犢獨讀賭鍍鍛斷緞兌隊對噸頓鈍奪墮鵝額訛惡餓兒爾餌貳發罰閥琺礬釩煩範販飯訪紡飛誹廢費紛墳奮憤糞豐楓鋒風瘋馮縫諷鳳膚輻撫輔賦複負訃婦縛該鈣蓋幹趕稈贛岡剛鋼綱崗臯鎬擱鴿閣鉻個給龔宮鞏貢鈎溝構購夠蠱顧剮關觀館慣貫廣規矽歸龜閨軌詭櫃貴劊輥滾鍋國過駭韓漢號閡鶴賀橫轟鴻紅後壺護滬戶嘩華畫劃話懷壞歡環還緩換喚瘓煥渙黃謊揮輝毀賄穢會燴彙諱誨繪葷渾夥獲貨禍擊機積饑譏雞績緝極輯級擠幾薊劑濟計記際繼紀夾莢頰賈鉀價駕殲監堅箋間艱緘繭檢堿鹼揀撿簡儉減薦檻鑒踐賤見鍵艦劍餞漸濺澗將漿蔣槳獎講醬膠澆驕嬌攪鉸矯僥腳餃繳絞轎較稭階節莖鯨驚經頸靜鏡徑痙競淨糾廄舊駒舉據鋸懼劇鵑絹傑潔結誡屆緊錦僅謹進晉燼盡勁荊覺決訣絕鈞軍駿開凱顆殼課墾懇摳庫褲誇塊儈寬礦曠況虧巋窺饋潰擴闊蠟臘萊來賴藍欄攔籃闌蘭瀾讕攬覽懶纜爛濫撈勞澇樂鐳壘類淚籬離裏鯉禮麗厲勵礫曆瀝隸倆聯蓮連鐮憐漣簾斂臉鏈戀煉練糧涼兩輛諒療遼鐐獵臨鄰鱗凜賃齡鈴淩靈嶺領餾劉龍聾嚨籠壟攏隴樓婁摟簍蘆盧顱廬爐擄鹵虜魯賂祿錄陸驢呂鋁侶屢縷慮濾綠巒攣孿灤亂掄輪倫侖淪綸論蘿羅邏鑼籮騾駱絡媽瑪碼螞馬罵嗎買麥賣邁脈瞞饅蠻滿謾貓錨鉚貿麽黴沒鎂門悶們錳夢謎彌覓冪綿緬廟滅憫閩鳴銘謬謀畝鈉納難撓腦惱鬧餒內擬膩攆撚釀鳥聶齧鑷鎳檸獰甯擰濘鈕紐膿濃農瘧諾歐鷗毆嘔漚盤龐賠噴鵬騙飄頻貧蘋憑評潑頗撲鋪樸譜棲淒臍齊騎豈啓氣棄訖牽扡釺鉛遷簽謙錢鉗潛淺譴塹槍嗆牆薔強搶鍬橋喬僑翹竅竊欽親寢輕氫傾頃請慶瓊窮趨區軀驅齲顴權勸卻鵲確讓饒擾繞熱韌認紉榮絨軟銳閏潤灑薩鰓賽三傘喪騷掃澀殺紗篩曬刪閃陝贍繕傷賞燒紹賒攝懾設紳審嬸腎滲聲繩勝聖師獅濕詩屍時蝕實識駛勢適釋飾視試壽獸樞輸書贖屬術樹豎數帥雙誰稅順說碩爍絲飼聳慫頌訟誦擻蘇訴肅雖隨綏歲孫損筍縮瑣鎖獺撻擡態攤貪癱灘壇譚談歎湯燙濤縧討騰謄銻題體屜條貼鐵廳聽烴銅統頭禿圖塗團頹蛻脫鴕馱駝橢窪襪彎灣頑萬網韋違圍爲濰維葦偉僞緯謂衛溫聞紋穩問甕撾蝸渦窩臥嗚鎢烏汙誣無蕪吳塢霧務誤錫犧襲習銑戲細蝦轄峽俠狹廈嚇鍁鮮纖鹹賢銜閑顯險現獻縣餡羨憲線廂鑲鄉詳響項蕭囂銷曉嘯蠍協挾攜脅諧寫瀉謝鋅釁興洶鏽繡虛噓須許敘緒續軒懸選癬絢學勳詢尋馴訓訊遜壓鴉鴨啞亞訝閹煙鹽嚴顔閻豔厭硯彥諺驗鴦楊揚瘍陽癢養樣瑤搖堯遙窯謠藥爺頁業葉醫銥頤遺儀彜蟻藝億憶義詣議誼譯異繹蔭陰銀飲隱櫻嬰鷹應纓瑩螢營熒蠅贏穎喲擁傭癰踴詠湧優憂郵鈾猶遊誘輿魚漁娛與嶼語籲禦獄譽預馭鴛淵轅園員圓緣遠願約躍鑰嶽粵悅閱雲鄖勻隕運蘊醞暈韻雜災載攢暫贊贓髒鑿棗竈責擇則澤賊贈紮劄軋鍘閘柵詐齋債氈盞斬輾嶄棧戰綻張漲帳賬脹趙蟄轍鍺這貞針偵診鎮陣掙睜猙爭幀鄭證織職執紙摯擲幟質滯鍾終種腫衆謅軸皺晝驟豬諸誅燭矚囑貯鑄築駐專磚轉賺樁莊裝妝壯狀錐贅墜綴諄著濁茲資漬蹤綜總縱鄒詛組鑽' ;
  Result := '' ;
  mCount := utf8Length( mStr ) ;
  for i := 1 to mCount do
    begin
    mchar := utf8Copy( mStr,i, 1 ) ;
    mat := utf8Pos( mchar,msc ) ;
    if mat > 0 then
       Result := Result + utf8Copy( mtc,mat,1 )
    else
       Result := Result + mchar ;
    end;
end;


function dgTrad2Simp( mStr : String ) : String ;
var
  mtc,msc,mchar : String ;
  mCount,i,mat : Integer ;
begin
  msc := '皑蔼碍爱翱袄奥坝罢摆败颁办绊帮绑镑谤剥饱宝报鲍辈贝钡狈备惫绷笔毕毙币闭边编贬变辩辫标鳖别瘪濒滨宾摈饼并拨钵铂驳卜补财参蚕残惭惨灿苍舱仓沧厕侧册测层诧搀掺蝉馋谗缠铲产阐颤场尝长偿肠厂畅钞车彻尘沉陈衬撑称惩诚骋痴迟驰耻齿炽冲虫宠畴踌筹绸丑橱厨锄雏础储触处传疮闯创锤纯绰辞词赐聪葱囱从丛凑蹿窜错达带贷担单郸掸胆惮诞弹当挡党荡档捣岛祷导盗灯邓敌涤递缔颠点垫电淀钓调迭谍叠钉顶锭订丢东动栋冻斗犊独读赌镀锻断缎兑队对吨顿钝夺堕鹅额讹恶饿儿尔饵贰发罚阀珐矾钒烦范贩饭访纺飞诽废费纷坟奋愤粪丰枫锋风疯冯缝讽凤肤辐抚辅赋复负讣妇缚该钙盖干赶秆赣冈刚钢纲岗皋镐搁鸽阁铬个给龚宫巩贡钩沟构购够蛊顾剐关观馆惯贯广规硅归龟闺轨诡柜贵刽辊滚锅国过骇韩汉号阂鹤贺横轰鸿红后壶护沪户哗华画划话怀坏欢环还缓换唤痪焕涣黄谎挥辉毁贿秽会烩汇讳诲绘荤浑伙获货祸击机积饥讥鸡绩缉极辑级挤几蓟剂济计记际继纪夹荚颊贾钾价驾歼监坚笺间艰缄茧检碱硷拣捡简俭减荐槛鉴践贱见键舰剑饯渐溅涧将浆蒋桨奖讲酱胶浇骄娇搅铰矫侥脚饺缴绞轿较秸阶节茎鲸惊经颈静镜径痉竞净纠厩旧驹举据锯惧剧鹃绢杰洁结诫届紧锦仅谨进晋烬尽劲荆觉决诀绝钧军骏开凯颗壳课垦恳抠库裤夸块侩宽矿旷况亏岿窥馈溃扩阔蜡腊莱来赖蓝栏拦篮阑兰澜谰揽览懒缆烂滥捞劳涝乐镭垒类泪篱离里鲤礼丽厉励砾历沥隶俩联莲连镰怜涟帘敛脸链恋炼练粮凉两辆谅疗辽镣猎临邻鳞凛赁龄铃凌灵岭领馏刘龙聋咙笼垄拢陇楼娄搂篓芦卢颅庐炉掳卤虏鲁赂禄录陆驴吕铝侣屡缕虑滤绿峦挛孪滦乱抡轮伦仑沦纶论萝罗逻锣箩骡骆络妈玛码蚂马骂吗买麦卖迈脉瞒馒蛮满谩猫锚铆贸么霉没镁门闷们锰梦谜弥觅幂绵缅庙灭悯闽鸣铭谬谋亩钠纳难挠脑恼闹馁内拟腻撵捻酿鸟聂啮镊镍柠狞宁拧泞钮纽脓浓农疟诺欧鸥殴呕沤盘庞赔喷鹏骗飘频贫苹凭评泼颇扑铺朴谱栖凄脐齐骑岂启气弃讫牵扦钎铅迁签谦钱钳潜浅谴堑枪呛墙蔷强抢锹桥乔侨翘窍窃钦亲寝轻氢倾顷请庆琼穷趋区躯驱龋颧权劝却鹊确让饶扰绕热韧认纫荣绒软锐闰润洒萨鳃赛叁伞丧骚扫涩杀纱筛晒删闪陕赡缮伤赏烧绍赊摄慑设绅审婶肾渗声绳胜圣师狮湿诗尸时蚀实识驶势适释饰视试寿兽枢输书赎属术树竖数帅双谁税顺说硕烁丝饲耸怂颂讼诵擞苏诉肃虽随绥岁孙损笋缩琐锁獭挞抬态摊贪瘫滩坛谭谈叹汤烫涛绦讨腾誊锑题体屉条贴铁厅听烃铜统头秃图涂团颓蜕脱鸵驮驼椭洼袜弯湾顽万网韦违围为潍维苇伟伪纬谓卫温闻纹稳问瓮挝蜗涡窝卧呜钨乌污诬无芜吴坞雾务误锡牺袭习铣戏细虾辖峡侠狭厦吓锨鲜纤咸贤衔闲显险现献县馅羡宪线厢镶乡详响项萧嚣销晓啸蝎协挟携胁谐写泻谢锌衅兴汹锈绣虚嘘须许叙绪续轩悬选癣绚学勋询寻驯训讯逊压鸦鸭哑亚讶阉烟盐严颜阎艳厌砚彦谚验鸯杨扬疡阳痒养样瑶摇尧遥窑谣药爷页业叶医铱颐遗仪彝蚁艺亿忆义诣议谊译异绎荫阴银饮隐樱婴鹰应缨莹萤营荧蝇赢颖哟拥佣痈踊咏涌优忧邮铀犹游诱舆鱼渔娱与屿语吁御狱誉预驭鸳渊辕园员圆缘远愿约跃钥岳粤悦阅云郧匀陨运蕴酝晕韵杂灾载攒暂赞赃脏凿枣灶责择则泽贼赠扎札轧铡闸栅诈斋债毡盏斩辗崭栈战绽张涨帐账胀赵蛰辙锗这贞针侦诊镇阵挣睁狰争帧郑证织职执纸挚掷帜质滞钟终种肿众诌轴皱昼骤猪诸诛烛瞩嘱贮铸筑驻专砖转赚桩庄装妆壮状锥赘坠缀谆着浊兹资渍踪综总纵邹诅组钻' ;
  mtc := '皚藹礙愛翺襖奧壩罷擺敗頒辦絆幫綁鎊謗剝飽寶報鮑輩貝鋇狽備憊繃筆畢斃幣閉邊編貶變辯辮標鼈別癟瀕濱賓擯餅並撥缽鉑駁蔔補財參蠶殘慚慘燦蒼艙倉滄廁側冊測層詫攙摻蟬饞讒纏鏟産闡顫場嘗長償腸廠暢鈔車徹塵沈陳襯撐稱懲誠騁癡遲馳恥齒熾沖蟲寵疇躊籌綢醜櫥廚鋤雛礎儲觸處傳瘡闖創錘純綽辭詞賜聰蔥囪從叢湊躥竄錯達帶貸擔單鄲撣膽憚誕彈當擋黨蕩檔搗島禱導盜燈鄧敵滌遞締顛點墊電澱釣調叠諜疊釘頂錠訂丟東動棟凍鬥犢獨讀賭鍍鍛斷緞兌隊對噸頓鈍奪墮鵝額訛惡餓兒爾餌貳發罰閥琺礬釩煩範販飯訪紡飛誹廢費紛墳奮憤糞豐楓鋒風瘋馮縫諷鳳膚輻撫輔賦複負訃婦縛該鈣蓋幹趕稈贛岡剛鋼綱崗臯鎬擱鴿閣鉻個給龔宮鞏貢鈎溝構購夠蠱顧剮關觀館慣貫廣規矽歸龜閨軌詭櫃貴劊輥滾鍋國過駭韓漢號閡鶴賀橫轟鴻紅後壺護滬戶嘩華畫劃話懷壞歡環還緩換喚瘓煥渙黃謊揮輝毀賄穢會燴彙諱誨繪葷渾夥獲貨禍擊機積饑譏雞績緝極輯級擠幾薊劑濟計記際繼紀夾莢頰賈鉀價駕殲監堅箋間艱緘繭檢堿鹼揀撿簡儉減薦檻鑒踐賤見鍵艦劍餞漸濺澗將漿蔣槳獎講醬膠澆驕嬌攪鉸矯僥腳餃繳絞轎較稭階節莖鯨驚經頸靜鏡徑痙競淨糾廄舊駒舉據鋸懼劇鵑絹傑潔結誡屆緊錦僅謹進晉燼盡勁荊覺決訣絕鈞軍駿開凱顆殼課墾懇摳庫褲誇塊儈寬礦曠況虧巋窺饋潰擴闊蠟臘萊來賴藍欄攔籃闌蘭瀾讕攬覽懶纜爛濫撈勞澇樂鐳壘類淚籬離裏鯉禮麗厲勵礫曆瀝隸倆聯蓮連鐮憐漣簾斂臉鏈戀煉練糧涼兩輛諒療遼鐐獵臨鄰鱗凜賃齡鈴淩靈嶺領餾劉龍聾嚨籠壟攏隴樓婁摟簍蘆盧顱廬爐擄鹵虜魯賂祿錄陸驢呂鋁侶屢縷慮濾綠巒攣孿灤亂掄輪倫侖淪綸論蘿羅邏鑼籮騾駱絡媽瑪碼螞馬罵嗎買麥賣邁脈瞞饅蠻滿謾貓錨鉚貿麽黴沒鎂門悶們錳夢謎彌覓冪綿緬廟滅憫閩鳴銘謬謀畝鈉納難撓腦惱鬧餒內擬膩攆撚釀鳥聶齧鑷鎳檸獰甯擰濘鈕紐膿濃農瘧諾歐鷗毆嘔漚盤龐賠噴鵬騙飄頻貧蘋憑評潑頗撲鋪樸譜棲淒臍齊騎豈啓氣棄訖牽扡釺鉛遷簽謙錢鉗潛淺譴塹槍嗆牆薔強搶鍬橋喬僑翹竅竊欽親寢輕氫傾頃請慶瓊窮趨區軀驅齲顴權勸卻鵲確讓饒擾繞熱韌認紉榮絨軟銳閏潤灑薩鰓賽三傘喪騷掃澀殺紗篩曬刪閃陝贍繕傷賞燒紹賒攝懾設紳審嬸腎滲聲繩勝聖師獅濕詩屍時蝕實識駛勢適釋飾視試壽獸樞輸書贖屬術樹豎數帥雙誰稅順說碩爍絲飼聳慫頌訟誦擻蘇訴肅雖隨綏歲孫損筍縮瑣鎖獺撻擡態攤貪癱灘壇譚談歎湯燙濤縧討騰謄銻題體屜條貼鐵廳聽烴銅統頭禿圖塗團頹蛻脫鴕馱駝橢窪襪彎灣頑萬網韋違圍爲濰維葦偉僞緯謂衛溫聞紋穩問甕撾蝸渦窩臥嗚鎢烏汙誣無蕪吳塢霧務誤錫犧襲習銑戲細蝦轄峽俠狹廈嚇鍁鮮纖鹹賢銜閑顯險現獻縣餡羨憲線廂鑲鄉詳響項蕭囂銷曉嘯蠍協挾攜脅諧寫瀉謝鋅釁興洶鏽繡虛噓須許敘緒續軒懸選癬絢學勳詢尋馴訓訊遜壓鴉鴨啞亞訝閹煙鹽嚴顔閻豔厭硯彥諺驗鴦楊揚瘍陽癢養樣瑤搖堯遙窯謠藥爺頁業葉醫銥頤遺儀彜蟻藝億憶義詣議誼譯異繹蔭陰銀飲隱櫻嬰鷹應纓瑩螢營熒蠅贏穎喲擁傭癰踴詠湧優憂郵鈾猶遊誘輿魚漁娛與嶼語籲禦獄譽預馭鴛淵轅園員圓緣遠願約躍鑰嶽粵悅閱雲鄖勻隕運蘊醞暈韻雜災載攢暫贊贓髒鑿棗竈責擇則澤賊贈紮劄軋鍘閘柵詐齋債氈盞斬輾嶄棧戰綻張漲帳賬脹趙蟄轍鍺這貞針偵診鎮陣掙睜猙爭幀鄭證織職執紙摯擲幟質滯鍾終種腫衆謅軸皺晝驟豬諸誅燭矚囑貯鑄築駐專磚轉賺樁莊裝妝壯狀錐贅墜綴諄著濁茲資漬蹤綜總縱鄒詛組鑽' ;
  Result := '' ;
  mCount := utf8Length( mStr ) ;
  for i := 1 to mCount do
    begin
    mchar := utf8Copy( mStr,i, 1 ) ;
    mat := utf8Pos( mchar,mtc ) ;
    if mat > 0 then
       Result := Result + utf8Copy( msc,mat,1 )
    else
       Result := Result + mchar ;
    end;
end;

function dgGetParaString(mText: String; mAt: Integer): String;
var
  mFDoc : TCSVDocument ;
begin
  Result := '' ;
  mFDoc := TCSVDocument.Create ;
  mFDoc.Delimiter := ',' ;
  mFDoc.QuoteChar := '"' ;
  // mFDoc.QuoteOuterWhitespace := False ;
  mFDoc.CSVText := mText ;
  if mFDoc.RowCount >= 1 then
    begin
      Result := mFDoc.Cells[ mAt - 1, 0 ] ;
    end;
  mFDoc.Free ;
end;

procedure dgCompressStream(mFromStream, mToStream: TMemoryStream);
var
  ms : Tcompressionstream ;
begin
  mFromStream.Position := 0 ;
  ms := Tcompressionstream.create( clMax,mToStream ) ;
  ms.CopyFrom( mFromStream,mFromStream.Size ) ;
  ms.Free ;
end;

procedure dgDeCompressStream(mFromStream, mToStream: TMemoryStream);
const
  BufferSize = 4096 ;
var
  ms : TDeCompressionStream ;
  count : Integer ;
  Buffer: array[0..BufferSize-1] of Byte;
begin
  mFromStream.Position := 0 ;
  ms := TDeCompressionStream.create( mFromStream ) ;
  try
    while True do
      begin
      Count := ms.Read(Buffer, BufferSize);
      if Count <> 0 then mToStream.WriteBuffer(Buffer, Count) else
        Break;
      end;
  finally
    ms.Free;
  end;
end;

function dgStreamReadLine( mStream : TMemoryStream ) : String  ;
var
  pChar : Char ;
  mCount : Integer ;
  mOk : Boolean ;
begin
  Result := '' ;
  mOk := True ;
  while mOk do
    begin
    mCount := mStream.Read( pChar, 1 ) ;
    if mCount = 0 then mOk := False ;
    if pChar <> #13 then
      Result := Result + pChar
    else
       mOk := False ;
    end;
end;


procedure dgCopyStream( mFrom, mTo : TMemoryStream ) ;
var
  lReadCount : LongInt ;
  mOk : Boolean ;
  lByte : Byte ;
  mBytes : Array[ 0..1024 ] of Byte ;
begin
  mOk := True ;
  while mOk do
    begin
    lReadCount := mFrom.Read( mBytes , 1024  ) ;
    if lReadCount > 0 then
      mTo.Write( mBytes, lReadCount ) ;
    if lReadCount < 1024 then
      mOk := False ;
    end;
end;

function dgStreamToDateTime( mMem : TMemoryStream ) : TDateTime ;
var
  mByte : Byte ; // 9
  mDateTime : TDateTime ;
begin
  mDateTime := 0 ;
  mMem.Read( mByte , 1 ) ;
  if mByte = 9 then
    mMem.Read( mDatetime, SizeOf( mDateTime ) ) ;
  Result := mDateTime ;
end;

procedure dgStreamAddLines( mStr : String ; mStream : TMemoryStream ) ;
var
  mLen : LongInt ;
  mRet : Char ;
begin
  mRet := #13 ;
  mLen := Length( mStr ) ;
  mStream.Write( mStr[1], mLen ) ;
  mStream.Write( mRet,1 ) ;
end;

procedure dgArrayOfVariantToStream(const mVar: array of Variant;
  mStream: TMemoryStream);
var
  mByte : Byte ;
  mLen, i : LongInt ;
  mMyVar : Variant ;
begin
  mByte := 88 ;
  mLen := Length( mVar ) ;
  mStream.Write( mByte, 1 ) ;
  mStream.Write( mLen, SizeOf( mLen ) ) ;
  for i := 0 to mLen - 1 do
    begin
    mMyVar := mVar[ i ] ;
    dgVariantToStream( mMyVar, mStream ) ;
    end;
end;

procedure dgVariantToStream( mVar : Variant ; mStream : TMemoryStream ) ;
var
  mByte : Byte ;
  mVarType : Integer ;
  mSize,mLen,i : LongInt ;
  // Supported Variable Type
  mVarString : String ;
  mVarInteger : Integer ;
  mVarInt64 : Int64 ;
  mVarSmallint : SmallInt ;
  mVarShortInt : ShortInt ;
  mVarWord : Word ;
  mVarLongWord : LongWord ;
  mVarCurrency : Currency ;
  mVarDateTime : TDateTime ;
  mVarBoolean : Boolean ;
  mVarSingle : Single ;
  mVarDouble : Double ;
  mText : TStringList ;
  ms : TMemoryStream ;
begin
  // Array of varByte   <----- is special case
  if VarIsArray( mVar ) then
    begin
    if VarType( mVar[ 0 ] ) = varByte then
      begin
      // TdgsMemTable's variable Array of varByte
      // TdgsMemTable's Change to Base64String Now
      mByte := 99 ;
      // +1 or not ?
      mStream.Write( mByte, 1 ) ;
      ms := TMemoryStream.Create ;
      VariantToStream( mVar, ms ) ;
      mSize := ms.Size ;
      ms.Position := 0 ;
      mStream.Write( mSize,SizeOf( mSize ) ) ;
      mStream.CopyFrom( ms, mSize ) ;
      ms.Free ;
      end
    else
      begin
      mByte := 88 ;
      mLen := VarArrayHighBound( mVar ,1 ) + 1 ;
      mStream.Write( mByte, 1 ) ;
      mStream.Write( mLen,SizeOf( mLen ) ) ;
      for i := 0 to mLen - 1 do
        dgVariantToStream( mVar[ i ], mStream ) ;
      end ;
    end
  else
    begin
    mVarType := VarType( mVar ) ;
    if mVarType = varString then
      begin
      mByte := 1 ;
      mVarString := mVar ;    // Force Type Casting
      ms := TMemoryStream.Create ;
      VariantToStream( dgStrToVariant( mVarString ) , ms ) ;
      ms.Position := 0 ;
      mLen := ms.Size ;
      mStream.Write( mByte, 1 ) ;
      mStream.Write( mLen,SizeOf( mLen ) ) ;
      mStream.CopyFrom( ms,mLen ) ;
      end
    else if mVarType = varOleStr then
      begin
      mByte := 2 ;
      mVarString := mVar ;    // Force Type Casting
      ms := TMemoryStream.Create ;
      VariantToStream( dgStrToVariant( mVarString ) , ms ) ;
      ms.Position := 0 ;
      mLen := ms.Size ;
      mStream.Write( mByte, 1 ) ;
      mStream.Write( mLen,SizeOf( mLen ) ) ;
      mStream.CopyFrom( ms,mLen ) ;
      end
    else if mVarType = varInteger then
      begin
      mByte := 2 ;
      mVarInteger := mVar ;
      mStream.Write( mByte, 1 ) ;
      mStream.Write( mVarInteger,SizeOf( mVarInteger ) ) ;
      end
    else if mVarType = varInt64 then
      begin
      mByte := 3 ;
      mVarInt64 := mVar ;
      mStream.Write( mByte, 1 ) ;
      mStream.Write( mVarInt64,SizeOf( mVarInt64 ) ) ;
      end
    else if mVarType = varSmallint then
      begin
      mByte := 4 ;
      mVarSmallint := mVar ;
      mStream.Write( mByte, 1 ) ;
      mStream.Write( mVarSmallint,SizeOf( mVarSmallint ) ) ;
      end
    else if mVarType = varShortInt then
      begin
      mByte := 5 ;
      mVarShortInt := mVar ;
      mStream.Write( mByte, 1 ) ;
      mStream.Write( mVarShortInt,SizeOf( mVarShortInt ) ) ;
      end
    else if mVarType = varWord then
      begin
      mByte := 6 ;
      mVarWord := mVar ;
      mStream.Write( mByte, 1 ) ;
      mStream.Write( mVarWord,SizeOf( mVarWord ) ) ;
      end
    else if mVarType = varLongWord then
      begin
      mByte := 7 ;
      mVarLongWord := mVar ;
      mStream.Write( mByte, 1 ) ;
      mStream.Write( mVarLongWord,SizeOf( mVarLongWord ) ) ;
      end
    else if mVarType = varCurrency then
      begin
      mByte := 8 ;
      mVarCurrency := mVar ;
      mStream.Write( mByte, 1 ) ;
      mStream.Write( mVarCurrency,SizeOf( mVarCurrency ) ) ;
      end
    else if mVarType = varDate then
      begin
      mByte := 9 ;
      mVarDateTime := mVar ;
      mStream.Write( mByte, 1 ) ;
      mStream.Write( mVarDateTime,SizeOf( mVarDateTime ) ) ;
      end
    else if mVarType = varBoolean then
      begin
      mByte := 10 ;
      mVarBoolean := mVar ;
      mStream.Write( mByte, 1 ) ;
      mStream.Write( mVarBoolean , 1  ) ;
      end
    else if mVarType = varSingle then
      begin
      mByte := 11 ;
      mVarSingle := mVar ;
      mStream.Write( mByte, 1 ) ;
      mStream.Write( mVarSingle , SizeOf( mVarSingle ) ) ;
      end
    else if mVarType = varDouble then
      begin
      mByte := 12 ;
      mVarDouble := mVar ;
      mStream.Write( mByte, 1 ) ;
      mStream.Write( mVarDouble , SizeOf( mVarDouble ) ) ;
      end
    else
      begin
      // ShowMessage( 'Error for Handling dgVariantToStream - Type : ' + dgVarType( mVar ) ) ;
      mByte := 77 ;
      mStream.Write( mByte, 1 ) ;
      end ;
    end;
end;



function StreamToVariant(stream:TStream):variant;
var
   p:PChar;
begin
     stream.Seek(0,{$ifdef LEVEL6}soBeginning{$else}0{$endif});
     Result:=VarArrayCreate([0,stream.Size - 1],VarByte);
     try
        p:=VarArrayLock(Result);
        try
           stream.ReadBuffer(p^,stream.Size);
        finally
           VarArrayUnlock(Result);
        end;
     except
        Result:=Unassigned;
     end;
end;

// Get contents of a variant and put it in a stream.
procedure VariantToStream(AVariant:variant; stream:TStream);
var
   p:PChar;
   sz:integer;
begin
     // Check if variant contains data and is an array.
     if VarIsEmpty(AVariant) or VarIsNull(AVariant) or (not VarIsArray(AVariant)) then exit;

     sz:=VarArrayHighBound(AVariant,1);
     p:=VarArrayLock(AVariant);
     try
        stream.WriteBuffer(p^,sz+1);
     finally
        VarArrayUnlock(AVariant);
     end;
end;

function dgStrToVariant( mStr : String ) : Variant ;
var
  ms : TMemoryStream ;
  mLen, i : LongInt ;
begin
  ms := TMemoryStream.Create ;
  mLen := Length( mStr ) ;
  ms.Write( mStr[ 1 ], mLen ) ;
  ms.Position := 0 ;
  Result := StreamToVariant( ms ) ;
  ms.Free ;
end;

function dgStreamToStr( mMem : TMemoryStream ) : String ;
var
  mByte : Byte ;
  mLen : LongInt ;
begin
  Result := '' ;
  mMem.Read( mByte, 1 ) ;
  if mByte = 1 then
    begin
    mMem.Read( mLen, SizeOf( mLen ) ) ;
    if mLen > 0 then
      begin
      SetLength( Result, mLen ) ;
      mMem.Read( Result[ 1 ], mLen ) ;
      end ;
    end ;
end;

procedure dgDateTimeToStream( mDateTime : TDateTime ; mMem : TMemoryStream ) ;
var
  mByte : Byte ; // 9
begin
  mByte := 9 ;
  mMem.Write( mByte, 1 ) ;
  mMem.Write( mDateTime , SizeOf( mDateTime ) ) ;
end;

procedure dgStrToStream(mStr: String; mMem: TMemoryStream);
var
  mByte : Byte ;
  ms : TMemoryStream ;
  mLen : LongInt ;
begin
  mByte := 1 ;
  ms := TMemoryStream.Create ;
  ms.Write( mStr[ 1 ], Length( mStr ) ) ;
  ms.Position := 0 ;
  mLen := ms.Size ;
  mMem.Write( mByte, 1 ) ;
  mMem.Write( mLen,SizeOf( mLen ) ) ;
  if mLen > 0 then
    mMem.CopyFrom( ms, mLen ) ;
  ms.Free ;
end;

procedure dgIntegerToStream(mInt: Integer; mMem: TMemoryStream);
var
  mByte : Byte ;
begin
  mByte := 2 ;
  mMem.Write( mByte, 1 ) ;
  mMem.Write( mInt,SizeOf( mInt ) ) ;
end;

function dgStreamToInteger(mMem: TMemoryStream): Integer;
var
  mByte : Byte ;
  mInt : Integer ;
begin
  mInt := 0 ;
  mMem.Read( mByte, 1 ) ;
  if mByte = 2 then
    mMem.Read( mInt,SizeOf( mInt ) ) ;
  Result := mInt ;
end;

procedure dgLongIntToStream(mLong: LongInt; mMem: TMemoryStream);
var
  mByte : Byte ;
begin
  mByte := 3 ;
  mMem.Write( mByte, 1 ) ;
  mMem.Write( mLong, SizeOf( mLong ) ) ;
end;

procedure dgFloatToStream(mFloat: Double; mMem: TMemoryStream);
var
  mByte : Byte ;
begin
   mByte := 12 ;
   mMem.Write( mByte, 1 ) ;
   mMem.Write( mFloat, SizeOf( mFloat ) ) ;
end;

procedure dgStreamCopyToStream(mSource, mDest: TMemoryStream; mSize: LongInt);
var
  mByte : Byte ;
  mLen : LongInt ;
begin
  mByte := 66 ;
  mDest.Write( mByte, 1 ) ;
  mDest.Write( mSize,SizeOf( mSize ) ) ;
  mDest.CopyFrom( mSource, mSize ) ;
end;

procedure dgStreamLoadFromStream(mSource, mDest: TMemoryStream);
var
  mByte : Byte ;
  mSize : LongInt ;
begin
  mSource.Read( mByte, 1 ) ;
  if mByte = 66 then
    begin
    mSource.Read( mSize, SizeOf( mSize ) ) ;
    mDest.CopyFrom( mSource, mSize ) ;
    end;
end;

procedure dgCurrencyToStream(mCurr: Currency; mMem: TMemoryStream);
var
  mByte : Byte ; //   8 ;
begin
  mByte := 8 ;
  mMem.Write( mByte, 1 ) ;
  mMem.Write( mCurr, SizeOf( mCurr ) ) ;
end;

procedure dgBooleanToStream(mBool: Boolean; mMem: TMemoryStream);
var
   mByte : Byte ; // 10
begin
   mByte := 10 ;
   mMem.Write( mByte, 1 ) ;
   mMem.Write( mBool, 1 ) ;
end;

function dgIntToBool(mInt: Integer): Boolean;
begin
  if mInt = 0 then Result := False else Result := True ;
end;

function dgBoolToInt(mBool: Boolean): Integer;
begin
  if mBool then Result := 1 else Result := 0 ;
end;

function dgStreamToLongInt(mMem: TMemoryStream): LongInt;
var
  mByte : Byte ;
  mLong : LongInt ;
begin
  mLong := 0 ;
  mMem.Read( mByte, 1 ) ;
  if mByte = 3 then
    mMem.Read( mLong,SizeOf( mLong ) ) ;
  Result := mLong ;
end;

function dgStreamToFloat(mMem: TMemoryStream): Double;
var
  mDouble : Double ;
  mByte : Byte ;
begin
  mDouble := 0 ;
  mMem.Read( mByte, 1 ) ;
  if mByte = 12 then
    mMem.Read( mDouble, SizeOf( mDouble ) ) ;
  Result := mDouble ;
end;

function dgStreamToCurrency(mMem: TMemoryStream): Currency;
var
  mByte : Byte ;
  mCurr : Currency ;
begin
  mCurr := 0 ;
  mMem.Read( mByte, 1 ) ;
  if mByte = 8 then
    mMem.Read( mCurr, SizeOf( mCurr ) ) ;
  Result := mCurr ;
end;

function dgStreamToBoolean(mMem: TMemoryStream): Boolean;
var
   mByte : Byte ; // 10
begin
   mMem.Read( mByte, 1 ) ;
   mMem.Read( Result, 1 ) ;
end;


end.

