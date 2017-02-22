// Fixture taken from https://github.com/jcingroup/C551608_Roki/blob/master/Work.WebProj/Scripts/src/tsx/m-parm.tsx

import $ = require('jquery');
import React = require('react');
import ReactDOM = require('react-dom');
import Moment = require('moment');
import ReactBootstrap = require("react-bootstrap");
import CommCmpt = require('comm-cmpt');
import CommFunc = require('comm-func');

namespace Parm {
    interface ParamData {
        Email?: string;
        PurchaseTotal?: number;
        HomoiothermyFee?: number;
        RefrigerFee?: number;
        AccountName?: string;
        BankName?: string;
        BankCode?: string;
        AccountNumber?: string;
        Fee?: number;
    }
    export class GridForm extends React.Component<any, { param?: ParamData }>{


        constructor() {

            super();
            this.queryInitData = this.queryInitData.bind(this);
            this.handleSubmit = this.handleSubmit.bind(this);
            this.componentDidMount = this.componentDidMount.bind(this);
            this.setInputValue = this.setInputValue.bind(this);
            this.render = this.render.bind(this);
            this.state = {
                param: {
                    Email: null,
                    PurchaseTotal: 0,
                    HomoiothermyFee: 0,
                    RefrigerFee:0,
                    AccountName: null,
                    BankName: null,
                    BankCode: null,
                    AccountNumber: null,
                    Fee: 0
                }
            }
        }
        static defaultProps = {
            apiInitPath: gb_approot + 'Active/ParmData/aj_ParamInit',
            apiPath: gb_approot + 'api/GetAction/PostParamData'
        }
        componentDidMount() {
            this.queryInitData();
        }
        queryInitData() {
            CommFunc.jqGet(this.props.apiInitPath, {})
                .done((data, textStatus, jqXHRdata) => {
                    this.setState({ param: data });
                })
                .fail((jqXHR, textStatus, errorThrown) => {
                    CommFunc.showAjaxError(errorThrown);
                });
        }
        handleSubmit(e: React.FormEvent) {

            e.preventDefault();
            CommFunc.jqPost(this.props.apiPath, this.state.param)
                .done((data, textStatus, jqXHRdata) => {
                    if (data.result) {
                        CommFunc.tosMessage(null, '修改完成', 1);
                    } else {
                        alert(data.message);
                    }
                })
                .fail((jqXHR, textStatus, errorThrown) => {
                    CommFunc.showAjaxError(errorThrown);
                });
            return;
        }
        handleOnBlur(date) {

        }
        setInputValue(name: string, e: React.SyntheticEvent) {
            let input: HTMLInputElement = e.target as HTMLInputElement;
            let obj = this.state.param;
            obj[name] = input.value;
            this.setState({ param: obj });
        }
        render() {

            var outHtml: JSX.Element = null;

            let param = this.state.param;
            let InputDate = CommCmpt.InputDate;

            outHtml = (
                <div>
    <ul className="breadcrumb">
        <li><i className="fa-list-alt"></i>
            {this.props.menuName}
            </li>
        </ul>
    <h4 className="title"> {this.props.caption} 基本資料維護</h4>
    <form className="form-horizontal" onSubmit={this.handleSubmit}>
        <div className="col-xs-12">
            <div className="item-box">
                {/*--email--*/}
                <div className="item-title text-center">
                <h5>Email信箱設定</h5>
                    </div>
                    <div className="alert alert-warning" role="alert">
                        <ol>
                            <li>多筆信箱請用「<strong className="text-danger">, </strong>」逗號分開。<br />ex.<strong>user1 @demo.com.tw, user2 @demo.com.tw</strong></li>
                            <li>Email 前面可填收件人姓名，用「<strong className="text-danger">: </strong>」冒號分隔姓名和信箱，此項非必要，可省略。<br />ex.<strong>收件人A: user1 @demo.com.tw, 收件人B: user2 @demo.com.tw</strong></li>
                            </ol>
                        </div>
                    <div className="form-group">
                       <label className="col-xs-1 control-label">收件信箱</label>
                       <div className="col-xs-9">
                                <input className="form-control" type="text"
                                    value={param.Email}
                                    onChange={this.setInputValue.bind(this, 'Email') }
                                    maxLength={500}
                                    required/>
                           </div>
                        </div>
                {/*--email end--*/}
                {/*--shoppingCost--*/}
                <div className="item-title text-center">
                <h5>訂單運費設定</h5>
                    </div>
                    <div className="form-group">
                       <label className="col-xs-3 control-label">會員下訂單，當訂單金額少於NT$</label>
                       <div className="col-xs-1">
                                <input className="form-control" type="number"
                                    value={param.PurchaseTotal}
                                    onChange={this.setInputValue.bind(this, 'PurchaseTotal') }
                                    min={0}
                                    required/>
                           </div>
                        <label className="col-xs-2 control-label">元時須付常溫運費NT$</label>
                       <div className="col-xs-1">
                                <input className="form-control" type="number"
                                    value={param.HomoiothermyFee}
                                    onChange={this.setInputValue.bind(this, 'HomoiothermyFee') }
                                    min={0}
                                    required/>
                           </div>
                        <label className="col-xs-2 control-label">元或冷凍(冷藏)運費NT$</label>
                       <div className="col-xs-1">
                                <input className="form-control" type="number"
                                    value={param.RefrigerFee}
                                    onChange={this.setInputValue.bind(this, 'RefrigerFee') }
                                    min={0}
                                    required/>
                           </div>
                        <label className="col-xs-1 control-label">元</label>
                        </div>

                {/*--shoppingCost end--*/}
                {/*--Payment--*/}
                <div className="item-title text-center">
                <h5>付款方式</h5>
                    </div>
                    <div className="form-group">
                     <label className="col-xs-4 control-label">當付款方式選擇『ATM轉帳』時，銀行帳號資料為: </label>
                        </div>
                    <div className="form-group">
                       <label className="col-xs-2 control-label">戶名: </label>
                       <div className="col-xs-3">
                                <input className="form-control" type="text"
                                    value={param.AccountName}
                                    onChange={this.setInputValue.bind(this, 'AccountName') }
                                    maxLength={16}
                                    required/>
                           </div>
                        </div>
                    <div className="form-group">
                       <label className="col-xs-2 control-label">銀行: </label>
                       <div className="col-xs-3">
                                <input className="form-control" type="text"
                                    value={param.BankName}
                                    onChange={this.setInputValue.bind(this, 'BankName') }
                                    maxLength={16}
                                    required/>
                           </div>
                        </div>
                    <div className="form-group">
                       <label className="col-xs-2 control-label">代碼: </label>
                       <div className="col-xs-3">
                                <input className="form-control" type="text"
                                    value={param.BankCode}
                                    onChange={this.setInputValue.bind(this, 'BankCode') }
                                    maxLength={5}
                                    required/>
                           </div>
                        </div>
                    <div className="form-group">
                       <label className="col-xs-2 control-label">帳號: </label>
                       <div className="col-xs-3">
                                <input className="form-control" type="text"
                                    value={param.AccountNumber}
                                    onChange={this.setInputValue.bind(this, 'AccountNumber') }
                                    maxLength={16}
                                    required/>
                           </div>
                        </div>
                    {/*<div className="form-group">
                     <label className="col-xs-4 control-label">當付款方式選擇『貨到付款』時，須加NT$ </label>
                       <div className="col-xs-1">
                                <input className="form-control" type="number"
                                    value={param.Fee}
                                    onChange={this.setInputValue.bind(this, 'Fee') }
                                    min={0}
                                    required/>
                           </div>
                     <label className="control-label">元手續費</label>
                        </div>*/}
                {/*--Payment end--*/}
                </div>



            <div className="form-action">
                <div className="col-xs-4 col-xs-offset-5">
                    <button type="submit" className="btn-primary"><i className="fa-check"></i> 儲存</button>
                    </div>
                </div>
            </div>
        </form>
                    </div>
            );

            return outHtml;
        }
    }
}

var dom = document.getElementById('page_content');
ReactDOM.render(<Parm.GridForm caption={gb_caption} menuName={gb_menuname} iconClass="fa-list-alt" />, dom);
